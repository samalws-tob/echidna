{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GADTs #-}

module Echidna.Exec where

import Control.Lens
import Control.Monad (forM_, when)
import Control.Monad.Catch (MonadThrow(..))
import Control.Monad.State.Strict (MonadState(get, put), execState, runStateT, MonadIO(liftIO))
import Control.Monad.Reader (MonadReader, asks)
import Data.Bits
import Data.ByteString qualified as BS
import Data.IORef (readIORef, atomicWriteIORef)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, fromJust)
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Vector.Unboxed.Mutable qualified as V
import System.Process (readProcessWithExitCode)

import EVM hiding (pc, Env, cache, contract, tx, value)
import EVM.ABI
import EVM.Exec (exec, vmForEthrunCreation)
import EVM.Fetch qualified
import EVM.Types (Expr(ConcreteBuf, Lit), hexText)

import Echidna.Events (emptyEvents)
import Echidna.RPC (safeFetchContractFrom, safeFetchSlotFrom)
import Echidna.Transaction
import Echidna.Types (ExecException(..), Gas, fromEVM, emptyAccount)
import Echidna.Types.Buffer (forceBuf)
import Echidna.Types.Coverage (CoverageMap)
import Echidna.Types.Signature (MetadataCache, getBytecodeMetadata, lookupBytecodeMetadata)
import Echidna.Types.Tx (TxCall(..), Tx, TxResult(..), call, dst, initialTimestamp, initialBlockNumber, getResult)
import Echidna.Types.Config (Env(..), EConfig(..), UIConf(..), OperationMode(..), OutputFormat(Text))
import Echidna.Types.Solidity (SolConf(..))
import Echidna.Utility (timePrefix)

-- | Broad categories of execution failures: reversions, illegal operations, and ???.
data ErrorClass = RevertE | IllegalE | UnknownE

-- | Given an execution error, classify it. Mostly useful for nice @pattern@s ('Reversion', 'Illegal').
classifyError :: Error -> ErrorClass
classifyError = \case
  OutOfGas _ _         -> RevertE
  Revert _             -> RevertE
  UnrecognizedOpcode _ -> RevertE
  StackLimitExceeded   -> RevertE
  StackUnderrun        -> IllegalE
  BadJumpDestination   -> IllegalE
  IllegalOverflow      -> IllegalE
  _                    -> UnknownE

-- | Extracts the 'Query' if there is one.
getQuery :: VMResult -> Maybe Query
getQuery (VMFailure (Query q)) = Just q
getQuery _                     = Nothing

-- | Matches execution errors that just cause a reversion.
pattern Reversion :: VMResult
pattern Reversion <- VMFailure (classifyError -> RevertE)

-- | Matches execution errors caused by illegal behavior.
pattern Illegal :: VMResult
pattern Illegal <- VMFailure (classifyError -> IllegalE)

-- | Given an execution error, throw the appropriate exception.
vmExcept :: MonadThrow m => Error -> m ()
vmExcept e = throwM $
  case VMFailure e of {Illegal -> IllegalExec e; _ -> UnknownFailure e}

-- | Given an error handler `onErr`, an execution strategy `executeTx`, and a transaction `tx`,
-- execute that transaction using the given execution strategy, calling `onErr` on errors.
execTxWith
  :: (MonadIO m, MonadState s m, MonadReader Env m)
  => Lens' s VM
  -> (Error -> m ())
  -> m VMResult
  -> Tx
  -> m (VMResult, Gas)
execTxWith l onErr executeTx tx = do
  vm <- use l
  if hasSelfdestructed vm tx.dst then
    pure (VMFailure (Revert (ConcreteBuf "")), 0)
  else do
    l . traces .= emptyEvents
    vmBeforeTx <- use l
    l %= execState (setupTx tx)
    gasLeftBeforeTx <- use $ l . state . gas
    vmResult <- runFully
    gasLeftAfterTx <- use $ l . state . gas
    handleErrorsAndConstruction vmResult vmBeforeTx
    pure (vmResult, gasLeftBeforeTx - gasLeftAfterTx)
  where
  runFully = do
    config <- asks (.cfg)
    -- TODO: Is the latest block a good default? It makes fuzzing hard to reproduce. Rethink this.
    let rpcBlock = maybe EVM.Fetch.Latest (EVM.Fetch.BlockNumber . fromIntegral) config.rpcBlock

    vmResult <- executeTx
    -- For queries, we halt execution because the VM needs some additional
    -- information from the outside. We provide this information and resume
    -- the execution by recursively calling `runFully`.
    case getQuery vmResult of
      -- A previously unknown contract is required
      Just q@(PleaseFetchContract addr continuation) -> do
        cacheRef <- asks (.fetchContractCache)
        cache <- liftIO $ readIORef cacheRef
        case Map.lookup addr cache of
          Just (Just contract) -> l %= execState (continuation contract)
          Just Nothing ->
            l %= execState (continuation emptyAccount)
          Nothing -> do
            logMsg $ "INFO: Performing RPC: " <> show q
            case config.rpcUrl of
              Just rpcUrl -> do
                ret <- liftIO $ safeFetchContractFrom rpcBlock rpcUrl addr
                case ret of
                  -- TODO: fix hevm to not return an empty contract in case of an error
                  Just contract | contract._contractcode /= EVM.RuntimeCode (EVM.ConcreteRuntimeCode "") -> do
                    metaCacheRef <- asks (.metadataCache)
                    metaCache <- liftIO $ readIORef metaCacheRef
                    let bc = forceBuf (contract ^. bytecode)
                    liftIO $ atomicWriteIORef metaCacheRef $ Map.insert bc (getBytecodeMetadata bc) metaCache

                    l %= execState (continuation contract)
                    liftIO $ atomicWriteIORef cacheRef $ Map.insert addr (Just contract) cache
                  _ -> do
                    -- TODO: better error reporting in HEVM, when intermmittent
                    -- network eror then retry
                    liftIO $ atomicWriteIORef cacheRef $ Map.insert addr Nothing cache
                    logMsg $ "ERROR: Failed to fetch contract: " <> show q
                    -- TODO: How should we fail here? It could be a network error,
                    -- RPC server returning junk etc.
                    l %= execState (continuation emptyAccount)
              Nothing -> do
                liftIO $ atomicWriteIORef cacheRef $ Map.insert addr Nothing cache
                logMsg $ "ERROR: Requested RPC but it is not configured: " <> show q
                -- TODO: How should we fail here? RPC is not configured but VM
                -- wants to fetch
                l %= execState (continuation emptyAccount)
        runFully -- resume execution

      -- A previously unknown slot is required
      Just q@(PleaseFetchSlot addr slot continuation) -> do
        cacheRef <- asks (.fetchSlotCache)
        cache <- liftIO $ readIORef cacheRef
        case Map.lookup addr cache >>= Map.lookup slot of
          Just (Just value) -> l %= execState (continuation value)
          Just Nothing -> l %= execState (continuation 0)
          Nothing -> do
            logMsg $ "INFO: Performing RPC: " <> show q
            case config.rpcUrl of
              Just rpcUrl -> do
                ret <- liftIO $ safeFetchSlotFrom rpcBlock rpcUrl addr slot
                case ret of
                  Just value -> do
                    l %= execState (continuation value)
                    liftIO $ atomicWriteIORef cacheRef $
                      Map.insertWith Map.union addr (Map.singleton slot (Just value)) cache
                  Nothing -> do
                    -- TODO: How should we fail here? It could be a network error,
                    -- RPC server returning junk etc.
                    logMsg $ "ERROR: Failed to fetch slot: " <> show q
                    liftIO $ atomicWriteIORef cacheRef $
                      Map.insertWith Map.union addr (Map.singleton slot Nothing) cache
                    l %= execState (continuation 0)
              Nothing -> do
                logMsg $ "ERROR: Requested RPC but it is not configured: " <> show q
                -- Use the zero slot
                l %= execState (continuation 0)
        runFully -- resume execution

      -- Execute a FFI call
      Just (PleaseDoFFI (cmd : args) continuation) -> do
        (_, stdout, _) <- liftIO $ readProcessWithExitCode cmd args ""
        let encodedResponse = encodeAbiValue $
              AbiTuple (V.fromList [AbiBytesDynamic . hexText . T.pack $ stdout])
        l %= execState (continuation encodedResponse)
        runFully

      -- No queries to answer, the tx is fully executed and the result is final
      _ -> pure vmResult

  -- | Handles reverts, failures and contract creations that might be the result
  -- (`vmResult`) of executing transaction `tx`.
  handleErrorsAndConstruction vmResult vmBeforeTx = case (vmResult, tx.call) of
    (Reversion, _) -> do
      tracesBeforeVMReset <- use $ l . traces
      codeContractBeforeVMReset <- use $ l . state . codeContract
      calldataBeforeVMReset <- use $ l . state . calldata
      callvalueBeforeVMReset <- use $ l . state . callvalue
      -- If a transaction reverts reset VM to state before the transaction.
      l .= vmBeforeTx
      -- Undo reset of some of the VM state.
      -- Otherwise we'd loose all information about the reverted transaction like
      -- contract address, calldata, result and traces.
      l . result ?= vmResult
      l . state . calldata .= calldataBeforeVMReset
      l . state . callvalue .= callvalueBeforeVMReset
      l . traces .= tracesBeforeVMReset
      l . state . codeContract .= codeContractBeforeVMReset
    (VMFailure x, _) -> onErr x
    (VMSuccess (ConcreteBuf bytecode'), SolCreate _) ->
      -- Handle contract creation.
      l %= execState (do
        env . contracts . at tx.dst . _Just . contractcode .= InitCode mempty mempty
        replaceCodeOfSelf (RuntimeCode (ConcreteRuntimeCode bytecode'))
        loadContract tx.dst)
    _ -> pure ()


logMsg :: (MonadIO m, MonadReader Env m) => String -> m ()
logMsg msg = do
  cfg <- asks (.cfg)
  operationMode <- asks (.cfg.uiConf.operationMode)
  when (operationMode == NonInteractive Text && not cfg.solConf.quiet) $ liftIO $ do
    time <- timePrefix
    putStrLn $ time <> msg

-- | Execute a transaction "as normal".
execTx
  :: (MonadIO m, MonadState VM m, MonadReader Env m, MonadThrow m)
  => Tx
  -> m (VMResult, Gas)
execTx = execTxWith id vmExcept $ fromEVM exec

-- | A type alias for the context we carry while executing instructions
type CoverageContext = (CoverageMap, Bool, Maybe (BS.ByteString, Int))

-- | Execute a transaction, logging coverage at every step.
execTxWithCov
  :: (MonadIO m, MonadState VM m, MonadReader Env m, MonadThrow m)
  => Tx -> CoverageMap
  -> m ((VMResult, Gas), (CoverageMap, Bool))
execTxWithCov tx cov = do
  vm <- get
  metaCacheRef <- asks (.metadataCache)
  cache <- liftIO $ readIORef metaCacheRef
  (r, (vm', (cm, grew, lastLoc))) <-
    runStateT (execTxWith _1 vmExcept (execCov cache) tx) (vm, (cov, False, Nothing))
  put vm'

  -- Update the last valid location with the transaction result
  grew' <- liftIO $ case lastLoc of
    Just (meta, pc) ->
      case Map.lookup meta cm of
        Nothing -> pure False -- shouldn't happen
        Just vec -> do
          let txResultBit = fromEnum $ getResult $ fst r
          V.read vec pc >>= \case
            (opIx, depths, txResults) | not (txResults `testBit` txResultBit) -> do
              V.write vec pc (opIx, depths, txResults `setBit` txResultBit)
              pure True -- we count this as new coverage
            _ -> pure False
    _ -> pure False

  pure (r, (cm, grew || grew'))
  where
    -- the same as EVM.exec but collects coverage, will stop on a query
    execCov cache = do
     (vm, cm) <- get
     (r, vm', cm') <- liftIO $ loop cache vm cm
     put (vm', cm')
     pure r

    -- | Repeatedly exec a step and add coverage until we have an end result
    loop :: MetadataCache -> VM -> CoverageContext -> IO (VMResult, VM, CoverageContext)
    loop cache !vm !cc = case vm._result of
      Nothing -> addCoverage cache vm cc >>= loop cache (stepVM vm)
      Just r -> pure (r, vm, cc)

    -- | Execute one instruction on the EVM
    stepVM :: VM -> VM
    stepVM = execState exec1

    -- | Add current location to the CoverageMap
    addCoverage :: MetadataCache -> VM -> CoverageContext -> IO CoverageContext
    addCoverage cache !vm (!cm, new, lastLoc) = do
      let (pc, opIx, depth) = currentCovLoc vm
          meta = currentMeta cache vm
      case Map.lookup meta cm of
        Nothing -> do
          let size = BS.length . forceBuf . view bytecode . fromJust $
                Map.lookup vm._state._contract vm._env._contracts
          if size > 0 then do
            vec <- V.new size
            -- We use -1 for opIx to indicate that the location was not covered
            forM_ [0..size-1] $ \i -> V.write vec i (-1, 0, 0)
            V.write vec pc (opIx, fromIntegral depth, 0 `setBit` fromEnum Stop)
            pure (Map.insert meta vec cm, True, Just (meta, pc))
          else do
            -- TODO: should we collect the coverage here? Even if there is no
            -- bytecode for external contract, we could have a "virtual" location
            -- that PC landed at and record that.
            pure (cm, new, lastLoc)
        Just vec -> do
          V.read vec pc >>= \case
            (_, depths, results) | depth < 64 && not (depths `testBit` depth) -> do
              V.write vec pc (opIx, depths `setBit` depth, results `setBit` fromEnum Stop)
              pure (cm, True, Just (meta, pc))
            _ ->
              pure (cm, new, Just (meta, pc))

    -- | Get the VM's current execution location
    currentCovLoc vm = (vm._state._pc, fromMaybe 0 $ vmOpIx vm, length vm._frames)

    -- | Get the current contract's bytecode metadata
    currentMeta cache vm = fromMaybe (error "no contract information on coverage") $ do
      buffer <- vm ^? env . contracts . at vm._state._contract . _Just . bytecode
      let bc = forceBuf buffer
      pure $ lookupBytecodeMetadata cache bc

initialVM :: Bool -> VM
initialVM ffi = vmForEthrunCreation mempty
  & block . timestamp .~ Lit initialTimestamp
  & block . number .~ initialBlockNumber
  & env . contracts .~ mempty -- fixes weird nonce issues
  & allowFFI .~ ffi
