{-# LANGUAGE TypeFamilies #-}

-- | BiP datatype and related instance for time-warp abstracted
-- serialization.

module Pos.Communication.BiP
       ( BiP(..)
       , biSer
       , biSerIO
       ) where

import           Universum

import           Control.Monad.ST hiding (stToIO)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Builder.Extra as Builder

import           Node.Message.Class (Serializable (..), hoistSerializable)
import qualified Node.Message.Decoder as TW

import           Pos.Binary.Class (Bi (..))
import qualified Pos.Binary.Class as Bi

data BiP = BiP

biPackMsg :: Bi.Encoding -> LBS.ByteString
biPackMsg = Builder.toLazyByteStringWith strategy mempty . Bi.toBuilder
  where
    strategy = Builder.untrimmedStrategy 1024 4096

type M = ST RealWorld

biUnpackMsg :: Bi t => Bi.Decoder RealWorld t -> TW.Decoder M t
biUnpackMsg decoder = TW.Decoder (fromBiDecoder Proxy (Bi.deserialiseIncremental decoder))

fromBiDecoder :: Bi t => Proxy t -> M (Bi.IDecode RealWorld t) -> M (TW.DecoderStep M t)
fromBiDecoder p x = do
    nextStep <- x
    case nextStep of
      (Bi.Partial cont)    -> return $ TW.Partial $ \bs -> TW.Decoder $ fromBiDecoder p (cont bs)
      (Bi.Done bs off t)   -> return (TW.Done bs off t)
      (Bi.Fail bs off exn) -> do
          let msg = "fromBiDecoder failure for " <> label p <> ": " <> show exn <> ", leftover: " <> show bs
          return (TW.Fail bs off msg)

biSer :: forall t. Bi t => Serializable BiP M t
biSer = Serializable packBi unpackBi
    where
    packBi :: t -> M LBS.ByteString
    packBi = pure . biPackMsg . encode

    unpackBi :: TW.Decoder M t
    unpackBi = biUnpackMsg Bi.decode

biSerIO :: forall t. Bi t => Serializable BiP IO t
biSerIO = hoistSerializable stToIO biSer
