{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-gadt-mono-local-binds #-}

module Echidna.SymExec (createSymTx) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent (ThreadId, forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (runReaderT)
import Data.ByteString.Lazy qualified as BS
import Data.Foldable (fold)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (singleton)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, mapMaybe, isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (toList)
import Optics.Core ((.~), (%))
import Echidna.Solidity (chooseContract)
import Echidna.Types (fromEVM)
import Echidna.Types.Campaign (CampaignConf(..))
import Echidna.Types.Config (EConfig(..))
import Echidna.Types.Solidity (SolConf(..))
import Echidna.Types.Signature (SolCall)
import EVM.ABI (AbiValue(..), Sig(..), decodeAbiValue)
import EVM.Expr (simplify)
import EVM.Fetch qualified as Fetch
import EVM.SMT (SMTCex(..), SMT2, assertProps)
import EVM (loadContract, resetState)
import EVM.Effects (defaultEnv, defaultConfig)
import EVM.Solidity (SolcContract(..), Method(..))
import EVM.Solvers (withSolvers, Solver(Z3), CheckSatResult(Sat), SolverGroup, checkSat)
import EVM.SymExec (interpret, runExpr, abstractVM, mkCalldata, LoopHeuristic (Naive), flattenExpr, extractProps)
import EVM.Types (Addr, VM(..), Frame(..), FrameState(..), VMType(..), Env(..), Expr(..), EType(..), BaseState(..), Query(..), Prop(..), BranchCondition(..), W256, word256Bytes, word)
import EVM.Traversals (mapExpr)
import Control.Monad.ST (stToIO, RealWorld)
import Control.Monad.State.Strict (execState, runStateT)
import Echidna.Types.Tx (Tx(..), TxCall(..), maxGasPerBlock)


-- | Uses symbolic execution to find transactions which would increase coverage.
-- Spawns a new thread; returns its thread ID as the first return value.
-- The second return value is an MVar which is populated with transactions
--   once the symbolic execution is finished.
createSymTx :: EConfig -> Maybe Text -> [SolcContract] -> Maybe SolCall -> VM Concrete RealWorld -> IO (ThreadId, MVar [Tx])
createSymTx cfg name cs call vm = do
  mainContract <- chooseContract cs name
  exploreContract cfg mainContract call vm

exploreContract :: EConfig -> SolcContract -> Maybe SolCall -> VM Concrete RealWorld -> IO (ThreadId, MVar [Tx])
exploreContract conf contract call vm = do
  let
    isConc = isJust call
    allMethods = Map.elems contract.abiMap
    concMethods callJust = filter (\method -> method.name == fst callJust) methods
    methods = maybe allMethods concMethods call
    timeout = Just (fromIntegral conf.campaignConf.symExecTimeout)
    maxIters = if isConc then Nothing else Just conf.campaignConf.symExecMaxIters
    askSmtIters = if isConc then 0 else conf.campaignConf.symExecAskSMTIters
    rpcInfo = Nothing

  threadIdChan <- newEmptyMVar
  doneChan <- newEmptyMVar
  resultChan <- newEmptyMVar

  flip runReaderT defaultEnv $ withSolvers Z3 (fromIntegral conf.campaignConf.symExecNSolvers) timeout $ \solvers -> do
    threadId <- liftIO $ forkIO $ flip runReaderT defaultEnv $ do
      res <- forM methods $ \method -> do
        let
          fetcher = concOrSymFetcher call solvers rpcInfo
          dst = conf.solConf.contractAddr
          calldata@(cd, constraints) = mkCalldata (Just (Sig method.methodSignature (snd <$> method.inputs))) []
          vmSym = abstractVM calldata contract.runtimeCode Nothing False
        vmSym' <- liftIO $ stToIO vmSym
        vmReset <- liftIO $ snd <$> runStateT (fromEVM resetState) vm
        let vm' = vmReset & execState (loadContract (LitAddr dst))
                          & vmMakeSymbolic
                          & #constraints .~ constraints
                          & #state % #callvalue .~ TxValue -- TODO
                          -- & #state % #caller .~ SymAddr "caller"
                          & #state % #calldata .~ cd
                          -- & #config % #baseState .~ AbstractBase
                          & #env % #contracts .~ (Map.union vmSym'.env.contracts vm.env.contracts)
        exprInter <- interpret fetcher maxIters askSmtIters Naive vm' runExpr
        models <- liftIO $ mapConcurrently (checkSat solvers) $ manipulateExprInter isConc exprInter
        pure $ mapMaybe (modelToTx dst method) models
      liftIO $ putMVar resultChan $ concat res
      liftIO $ putMVar doneChan ()
    liftIO $ putMVar threadIdChan threadId
    liftIO $ takeMVar doneChan

  threadId <- takeMVar threadIdChan
  pure (threadId, resultChan)

manipulateExprInter :: Bool -> Expr End -> [SMT2]
manipulateExprInter isConc = map (assertProps defaultConfig) . middleStep . map (extractProps . simplify) . flattenExpr . simplify where
  middleStep = if isConc then middleStepConc else id
  middleStepConc = map singleton . concatMap (go (PBool True))
  go :: Prop -> [Prop] -> [Prop]
  go _ [] = []
  go acc (h:t) = (PNeg h `PAnd` acc):(go (h `PAnd` acc) t)

-- | Sets result to Nothing, and sets gas to ()
vmMakeSymbolic :: VM Concrete s -> VM Symbolic s
vmMakeSymbolic vm
  = VM
  { result         = Nothing
  , state          = frameStateMakeSymbolic vm.state
  , frames         = map frameMakeSymbolic vm.frames
  , env            = vm.env
  , block          = vm.block
  , tx             = vm.tx
  , logs           = vm.logs
  , traces         = vm.traces
  , cache          = vm.cache
  , burned         = ()
  , iterations     = vm.iterations
  , constraints    = vm.constraints
  , config         = vm.config
  , forks          = vm.forks
  , currentFork    = vm.currentFork
  }

frameStateMakeSymbolic :: FrameState Concrete s -> FrameState Symbolic s
frameStateMakeSymbolic fs
  = FrameState
  { contract     = fs.contract
  , codeContract = fs.codeContract
  , code         = fs.code
  , pc           = fs.pc
  , stack        = fs.stack
  , memory       = fs.memory
  , memorySize   = fs.memorySize
  , calldata     = fs.calldata
  , callvalue    = fs.callvalue
  , caller       = fs.caller
  , gas          = ()
  , returndata   = fs.returndata
  , static       = fs.static
  }

frameMakeSymbolic :: Frame Concrete s -> Frame Symbolic s
frameMakeSymbolic fr = Frame { context = fr.context, state = frameStateMakeSymbolic fr.state }

modelToTx :: Addr -> Method -> CheckSatResult -> Maybe Tx
modelToTx dst method result =
  case result of
    Sat cex ->
      let
        args = (zip [1..] method.inputs) <&> \(i::Int, (_argName, argType)) ->
          case Map.lookup (Var ("arg" <> T.pack (show i))) cex.vars of
            Just w ->
              decodeAbiValue argType (BS.fromStrict (word256Bytes w))
            Nothing -> -- put a placeholder
              decodeAbiValue argType (BS.repeat 0)

        value = fromMaybe 0 $ Map.lookup TxValue cex.txContext

      in Just Tx
        { call = SolCall (method.name, args)
        , src = 0
        , dst = dst
        , gasprice = 0
        , gas = maxGasPerBlock
        , value = value
        , delay = (0, 0)
        }

    _ -> Nothing

type Substs = ([(Text, W256)], [(Text, Addr)])

genSubsts :: SolCall -> Substs
genSubsts (_, abiVals) = fold $ zipWith genVal abiVals (T.pack . ("arg" <>) . show <$> ([1..] :: [Int])) where
  genVal (AbiUInt _ i) name = ([(name, fromIntegral i)], [])
  genVal (AbiInt _ i) name = ([(name, fromIntegral i)], [])
  genVal (AbiBool b) name = ([(name, if b then 1 else 0)], [])
  genVal (AbiAddress addr) name = ([], [(name, addr)])
  genVal (AbiBytes n b) name | n > 0 && n <= 32 = ([(name, word b)], [])
  genVal (AbiArray _ _ vals) name = fold $ zipWith genVal (toList vals) [name <> T.pack (show n) | n <- [0..] :: [Int]]
  genVal _ _ = error "TODO: not yet implemented"

substExpr :: Substs -> Expr a -> Expr a
substExpr (sw, sa) = mapExpr go where
  go v@(Var t) = maybe v Lit (lookup t sw)
  go v@(SymAddr t) = maybe v LitAddr (lookup t sa)
  go e = e

concFetcher :: Substs -> SolverGroup -> Fetch.RpcInfo -> Fetch.Fetcher t m s
concFetcher substs s r (PleaseAskSMT branchcondition pathconditions continue) =
  case simplify (substExpr substs branchcondition) of
    Lit n -> continue <$> pure (Case (n/=0))
    simplifiedExpr -> Fetch.oracle s r (PleaseAskSMT simplifiedExpr pathconditions continue)
concFetcher _ s r q = Fetch.oracle s r q

concOrSymFetcher :: Maybe SolCall -> SolverGroup -> Fetch.RpcInfo -> Fetch.Fetcher t m s
concOrSymFetcher (Just c) = concFetcher $ genSubsts c
concOrSymFetcher Nothing = Fetch.oracle
