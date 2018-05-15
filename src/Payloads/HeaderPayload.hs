module Payloads.HeaderPayload where

import Data.Binary

import Types

data HeaderPayload = HP LongLong | HP2 Short LongLong deriving Show


instance Binary HeaderPayload where
    put (HP size) = putShort 60 >> putShort 0 >> put size >> putShort 0
    get = do
        a <- get
        _ <- get :: (Get Short)
        size <- get
        _ <- get :: (Get Short)
        return $ HP2 a size
