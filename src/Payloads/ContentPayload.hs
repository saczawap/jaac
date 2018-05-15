module Payloads.ContentPayload where

import qualified Data.ByteString as BS
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get

import qualified Data.ByteString.Lazy as BL

import Types

data ContentPayload = CP BL.ByteString deriving Show

instance Binary ContentPayload where
    put (CP bs) = putLazyByteString bs
    get = do
        size <- (get :: Get Size)
        bs <- getLazyByteString $ fromIntegral size
        return $ CP bs