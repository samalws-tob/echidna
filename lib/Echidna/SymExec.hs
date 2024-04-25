{-# LANGUAGE DataKinds #-}

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
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, mapMaybe, isNothing, listToMaybe, fromJust)
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
import EVM.Expr (simplify, simplifyProp)
import EVM.Fetch qualified as Fetch
import EVM.SMT (SMTCex(..))
import EVM (loadContract, resetState)
import EVM.Effects (defaultEnv)
import EVM.Solidity (SolcContract(..), Method(..))
import EVM.Solvers (withSolvers, Solver(Z3), CheckSatResult(Sat), SolverGroup)
import EVM.SymExec (interpret, runExpr, abstractVM, mkCalldata, produceModels, LoopHeuristic (Naive), flattenExpr)
import EVM.Types (Addr, VM(..), Frame(..), FrameState(..), VMType(..), Env(..), Expr(..), EType(..), BaseState(..), EvmError(..), Query(..), Prop(..), BranchCondition(..), W256, word256Bytes, (./=), word)
import EVM.Traversals (mapExpr)
import Control.Monad.ST (stToIO, RealWorld)
import Control.Monad.State.Strict (execState, runStateT)
import Echidna.Types.Tx (Tx(..), TxCall(..), maxGasPerBlock)


-- | Uses symbolic execution to find transactions which would increase coverage.
-- Spawns a new thread; returns its thread ID as the first return value.
-- The second return value is an MVar which is populated with transactions
--   once the symbolic execution is finished.
createSymTx :: EConfig -> Maybe Text -> [SolcContract] -> SolCall -> VM Concrete RealWorld -> IO (ThreadId, MVar [Tx])
createSymTx cfg name cs call vm = do
  mainContract <- chooseContract cs name
  exploreContract cfg mainContract call vm

exploreContract :: EConfig -> SolcContract -> SolCall -> VM Concrete RealWorld -> IO (ThreadId, MVar [Tx])
exploreContract conf contract call vm = do
  let methods = Map.elems contract.abiMap
      method_ = listToMaybe $ filter (\method -> method.name == fst call) methods
      method = fromJust method_ -- only evaluated after checking for isNothing
      timeout = Just (fromIntegral conf.campaignConf.symExecTimeout)

  threadIdChan <- newEmptyMVar
  doneChan <- newEmptyMVar
  resultChan <- newEmptyMVar

  if isNothing method_ then pure () else
    flip runReaderT defaultEnv $ withSolvers Z3 (fromIntegral conf.campaignConf.symExecNSolvers) timeout $ \solvers -> do
      threadId <- liftIO $ forkIO $ flip runReaderT defaultEnv $ do
        ----liftIO $ print method
        let
          dst = conf.solConf.contractAddr
          calldata@(cd, constraints) = mkCalldata (Just (Sig method.methodSignature (snd <$> method.inputs))) []
          vmSym = abstractVM calldata contract.runtimeCode Nothing False
          maxIters = Just conf.campaignConf.symExecMaxIters
          askSmtIters = conf.campaignConf.symExecAskSMTIters
          rpcInfo = Nothing
        vmSym' <- liftIO $ stToIO vmSym
        vmReset <- liftIO $ snd <$> runStateT (fromEVM resetState) vm
        let vm' = vmReset & execState (loadContract (LitAddr dst))
                          & vmMakeSymbolic
                          & #frames .~ []
                          & #constraints .~ constraints
                          & #state % #callvalue .~ TxValue
                          -- & #state % #caller .~ SymAddr "caller"
                          & #state % #calldata .~ cd
                          & #state % #pc .~ 0
                          & #state % #stack .~ []
                          & #config % #baseState .~ AbstractBase
                          & #env % #contracts .~ (Map.union vmSym'.env.contracts vm.env.contracts)
        ----liftIO $ putStrLn "interpreting"
        exprInter <- interpret (myFetcher solvers rpcInfo (genSubsts call)) maxIters askSmtIters Naive vm' runExpr
        ----liftIO $ putStrLn "after interpreting"
        --liftIO $ putStrLn "doing one foreach before"
        --liftIO $ mapM_ (const $ putStrLn "A") (flattenExpr (simplify exprInter))
        --liftIO $ mapM_ (const $ putStrLn "A") (flattenExpr exprInter)
        --liftIO $ putStrLn "before vs after" >> print (length (flattenExpr (simplify exprInter))) >> print (length (manipulateExprInter exprInter))
        let asdf = manipulateExprInter1 exprInter
        let bsdf = concatMap manipulateExprInter2 $ map simplify $ flattenExpr $ simplify exprInter
        ----liftIO $ putStrLn "lena is" >> print (length asdf)
        ----liftIO $ putStrLn "lenb is" >> print (length bsdf)
        ----liftIO $ putStrLn "bsdf is" >> print ((\(Success p _ _ _) -> p) <$> bsdf)
        res <- liftIO $ flip mapConcurrently bsdf $ (\m -> {-putStrLn "producing model" >>-} flip runReaderT defaultEnv (produceModels solvers m) >>= (\r -> {-putStrLn "done producing a model" >>-} pure r))
        ----liftIO $ putStrLn "done symbexing"
        let ress = mapMaybe (modelToTx dst method) $ concat res
        ----liftIO $ putStrLn "ress is"
        ----liftIO $ print $ (.call) <$> ress

        liftIO $ putMVar resultChan ress
        liftIO $ putMVar doneChan ()
      liftIO $ putMVar threadIdChan threadId
      liftIO $ takeMVar doneChan

  threadId <- takeMVar threadIdChan
  pure (threadId, resultChan)

manipulateExprInter1 = map simplify . filter filterFn . flattenExpr . simplify where
  filterFn (Failure _ _ (Revert _)) = False
  filterFn _ = True

manipulateExprInter2 :: Expr End -> [Expr End]
manipulateExprInter2 e = (\p -> Success [p] undefined undefined undefined {- TODO can we just go from props instead of doing this -}) <$> go (PBool True) props where
  props :: [Prop]
  props = case e of
            Partial p _ _ -> p
            Failure p _ _ -> p
            Success p _ _ _ -> p
            ITE _ _ _ -> error "shouldnt reach here, TODO better msg"
  go acc [] = []
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

modelToTx :: Addr -> Method -> (Expr 'End, CheckSatResult) -> Maybe Tx
modelToTx dst method (_end, result) =
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
genSubsts (_, abiVals) = fold $ zipWith genVal abiVals (T.pack . ("arg"<>) . show <$> [1..]) where
  genVal (AbiUInt _ i) name = ([(name, fromIntegral i)], [])
  genVal (AbiInt _ i) name = ([(name, fromIntegral i)], [])
  genVal (AbiBool b) name = ([(name, if b then 1 else 0)], [])
  genVal (AbiAddress addr) name = ([], [(name, addr)])
  genVal (AbiBytes n b) name | n > 0 && n <= 32 = ([(name, word b)], [])
  genVal (AbiArray _ _ vals) name = fold $ zipWith genVal (toList vals) [name <> T.pack (show n) | n <- [0..]]
  genVal _ _ = error "TODO: not yet implemented"

substExpr :: Substs -> Expr a -> Expr a
substExpr (sw, sa) = mapExpr go where
  go v@(Var t) = maybe v Lit (lookup t sw)
  go v@(SymAddr t) = maybe v LitAddr (lookup t sa)
  go e = e

myFetcher :: SolverGroup -> Fetch.RpcInfo -> Substs -> Fetch.Fetcher t m s
myFetcher s r substs (PleaseAskSMT branchcondition pathconditions continue) =
  case simplify (substExpr substs branchcondition) of
    Lit n -> continue <$> pure (Case (n/=0))
    simplifiedExpr -> Fetch.oracle s r (PleaseAskSMT simplifiedExpr pathconditions continue)
myFetcher s r _ q = Fetch.oracle s r q
