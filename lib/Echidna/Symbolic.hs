{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Echidna.Symbolic where

import Data.ByteString (ByteString)
import EVM.Types (Expr(..), EType(..), W256, Addr, Contract(..), ContractCode(..), RuntimeCode(..))
import EVM.Expr qualified as Expr

forceBuf :: Expr Buf -> ByteString
forceBuf b = case b of
  ConcreteBuf b' -> b'
  _ -> error $ "expected ConcreteBuf: " <> show b

forceWord :: Expr EWord -> W256
forceWord x = case x of
  Lit x' -> x'
  WAddr x' -> fromIntegral $ forceAddr x'
  _ -> error $ "expected Lit: " <> show x

forceAddr :: Expr EAddr -> Addr
forceAddr x = case x of
  LitAddr x' -> x'
  _ -> error $ "expected LitAddr: " <> show x

bytecode :: Contract -> ByteString
bytecode = f . (.code)
  where f (InitCode bs _) = bs
        f (RuntimeCode (ConcreteRuntimeCode bs)) = bs
        f (RuntimeCode (SymbolicRuntimeCode ops)) = forceBuf $ Expr.fromList ops 
        f (UnknownCode _) = error "TODO"
