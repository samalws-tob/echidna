{-# LANGUAGE GADTs #-}

module Echidna.Types.Signature where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Foldable (find)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.IORef (IORef, readIORef, atomicWriteIORef)

import EVM.ABI (AbiType, AbiValue)
import EVM.Types (Addr, W256)
import Data.Map (Map)

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

-- | Used to memoize results of getBytecodeMetadata
type MetadataCache = Map W256 ByteString

type SignatureMap = Map ByteString (NonEmpty SolSignature)

getBytecodeMetadata :: ByteString -> ByteString
getBytecodeMetadata bs =
  let stripCandidates = flip BS.breakSubstring bs <$> knownBzzrPrefixes in
    case find ((/= mempty) . snd) stripCandidates of
      Nothing     -> bs -- if no metadata is found, return the complete bytecode
      Just (_, m) -> m

lookupBytecodeMetadata :: MetadataCache -> W256 -> ByteString -> ByteString
lookupBytecodeMetadata memo codehash bs = fromMaybe (getBytecodeMetadata bs) (memo M.!? codehash)

lookupBytecodeMetadataIO :: (MonadIO m) => IORef MetadataCache -> W256 -> ByteString -> m ByteString
lookupBytecodeMetadataIO memoRef codehash bs = (\memo -> maybe (let meta = getBytecodeMetadata bs in liftIO (atomicWriteIORef memoRef (M.insert codehash meta memo)) >> pure meta) pure (memo M.!? codehash)) =<< liftIO (readIORef memoRef)

-- | Precalculate getBytecodeMetadata for all contracts in a list
makeBytecodeCache :: [(W256, ByteString)] -> MetadataCache
makeBytecodeCache bss = M.fromList $ fmap getBytecodeMetadata <$> bss

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
