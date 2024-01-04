{-# LANGUAGE GADTs #-}

module Echidna.Types.Signature where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Foldable (find)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Maybe (fromJust)
import Data.IORef (IORef, readIORef, atomicWriteIORef, newIORef, atomicModifyIORef')

import EVM.ABI (AbiType, AbiValue)
import EVM.Types (Addr, W256)
import Data.Map (Map)
import Data.Map qualified as Map

-- | Name of the contract
type ContractName = Text

-- | Name of a function
type FunctionName = Text

-- | Represents the type of a Solidity function.
-- A tuple for the name of the function and the 'AbiType's of any arguments it expects.
type SolSignature = (FunctionName, [AbiType])

-- | Represents a call to a Solidity function.
-- A tuple for the name of the function and then any 'AbiValue' arguments passed (as a list).
type SolCall = (FunctionName, [AbiValue])

-- | A contract is just an address with an ABI (for our purposes).
type ContractA = (Addr, NonEmpty SolSignature)

type BytecodeMetadataID = Int

-- | Used to memoize results of getBytecodeMetadata
data MetadataCacheInside = MetadataCacheInside !(Map W256 BytecodeMetadataID) !(Map ByteString BytecodeMetadataID) !(Map BytecodeMetadataID ByteString) !Int

newtype MetadataCacheRef = MetadataCacheRef (IORef MetadataCacheInside)

type SignatureMap = Map BytecodeMetadataID (NonEmpty SolSignature)

getBytecodeMetadata_renamed :: ByteString -> ByteString
getBytecodeMetadata_renamed bs =
  let stripCandidates = flip BS.breakSubstring bs <$> knownBzzrPrefixes in
    case find ((/= mempty) . snd) stripCandidates of
      Nothing     -> bs -- if no metadata is found, return the complete bytecode
      Just (_, m) -> m

{-
lookupBytecodeMetadata :: MetadataCache -> W256 -> ByteString -> BytecodeMetadataID
lookupBytecodeMetadata memo codehash bs = fromMaybe (getBytecodeMetadata_renamed bs) (memo M.!? codehash)
-}

lookupBytecodeMetadataIO :: (MonadIO m) => MetadataCacheRef -> W256 -> ByteString -> m BytecodeMetadataID
-- lookupBytecodeMetadataIO = cacheMeta -- TODO -- (MetadataCacheRef memoRef) codehash bs = (\memo -> maybe (let meta = BytecodeMetadataID (getBytecodeMetadata_renamed bs) in liftIO (atomicWriteIORef memoRef (MetadataCacheInside $ M.insert codehash meta memo)) >> pure meta) pure (memo M.!? codehash)) =<< liftIO (unMetadataCacheInside <$> readIORef memoRef)
lookupBytecodeMetadataIO r@(MetadataCacheRef ref) codehash bs = do
  MetadataCacheInside to _ _ _ <- liftIO $ readIORef ref
  case (Map.lookup codehash to) of
    (Just res) -> pure res
    Nothing -> cacheMeta r codehash bs

unBytecodeMetadataID :: (MonadIO m) => MetadataCacheRef -> BytecodeMetadataID -> m ByteString
unBytecodeMetadataID (MetadataCacheRef ref) idLookingFor = do
  MetadataCacheInside _ _ from _ <- liftIO $ readIORef ref
  pure $ fromJust $ Map.lookup idLookingFor from

{-
cacheMeta :: (MonadIO m) => MetadataCacheRef -> W256 -> ByteString -> m ()
cacheMeta (MetadataCacheRef metaCacheRef) codehash bs = do
  metaCache <- liftIO $ readIORef metaCacheRef
  let a = toIDMap metaCache
  let b = fromIDMap metaCache
  liftIO $ atomicWriteIORef metaCacheRef $ MetadataCacheInside $ insert codehash (BytecodeMetadataID $ getBytecodeMetadata_renamed bs) metaCache
-}
-- returns the id
cacheMeta :: (MonadIO m) => MetadataCacheRef -> W256 -> ByteString -> m BytecodeMetadataID
cacheMeta (MetadataCacheRef metaCacheRef) codehash bs = liftIO $ atomicModifyIORef' metaCacheRef f where
  f old@(MetadataCacheInside to _ _ _) = case (Map.lookup codehash to) of
    (Just val) -> (old, val)
    Nothing -> g old
  g (MetadataCacheInside to to2 from highest) = let bcm = getBytecodeMetadata_renamed bs in case (Map.lookup bcm to2) of
    (Just val) -> (modifyLittle to to2 from highest bcm val, val)
    Nothing -> modify to to2 from highest bcm
  modify to to2 from highest bcm = let newid = highest+1 in (,newid) $ MetadataCacheInside (Map.insert codehash newid to) (Map.insert bcm newid to2) (Map.insert newid bcm from) newid
  modifyLittle to to2 from highest bcm val = MetadataCacheInside (Map.insert codehash val to) to2 from highest

-- | Precalculate getBytecodeMetadata for all contracts in a list
initBytecodeCache :: (MonadIO m) => MetadataCacheRef -> [(W256, ByteString)] -> m ()
-- initBytecodeCache (MetadataCacheRef ref) bss = liftIO $ atomicWriteIORef ref $ MetadataCacheInside $ M.fromList $ fmap (BytecodeMetadataID . getBytecodeMetadata_renamed) <$> bss
initBytecodeCache ref bss = mapM_ (\(a,b) -> cacheMeta ref a b) bss

newMetadataCacheRef :: (MonadIO m) => m MetadataCacheRef
newMetadataCacheRef = liftIO $ fmap MetadataCacheRef $ newIORef $ MetadataCacheInside mempty mempty mempty 0

knownBzzrPrefixes :: [ByteString]
knownBzzrPrefixes =
  -- a1 65 "bzzr0" 0x58 0x20 (solc <= 0.5.8)
  [ BS.pack [0xa1, 0x65, 98, 122, 122, 114, 48, 0x58, 0x20]
  -- a2 65 "bzzr0" 0x58 0x20 (solc >= 0.5.9)
  , BS.pack [0xa2, 0x65, 98, 122, 122, 114, 48, 0x58, 0x20]
  -- a2 65 "bzzr1" 0x58 0x20 (solc >= 0.5.11)
  , BS.pack [0xa2, 0x65, 98, 122, 122, 114, 49, 0x58, 0x20]
  -- a2 64 "ipfs" 0x58 0x22 (solc >= 0.6.0)
  , BS.pack [0xa2, 0x64, 0x69, 0x70, 0x66, 0x73, 0x58, 0x22]
  ]
