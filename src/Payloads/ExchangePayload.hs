module Payloads.ExchangePayload where

import Data.Binary

import Types

data ExchangePayload = 
    Declare {
        name :: ExchangeName,
        exchangeType :: ShortString,
        passive :: Bit, 
        durable :: Bit,
        noWait :: NoWait,
        arguments :: FieldTable } | 
    DeclareOk 
    deriving Show


instance Binary ExchangePayload where
    put (Declare a b c d e f) = putShort 10 >> putShort 0 >> put a >> put b >> (put (0 :: Word8)) >> put f
    get = get >>= getExchangePayload

getExchangePayload :: MethodId -> Get ExchangePayload
getExchangePayload 11 = return $ DeclareOk
