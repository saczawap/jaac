module Payloads.QueuePayload where

import Data.Binary

import Types

data QueuePayload = 
    Declare {
        queueName :: QueueName,
        passive :: Bit,
        durable :: Bit,
        exclusive :: Bit,
        autoDelete :: Bit,
        noWait :: NoWait,
        arguments :: FieldTable } | 
    DeclareOk {
        queueName :: QueueName,    
        messageCount :: MessageCount,
        consumerCount :: Long } | 
    Bind {
        queueName :: QueueName,    
        exchangeName :: ExchangeName,    
        routingKey :: ShortString,
        noWait :: NoWait,
        arguments :: FieldTable } | 
    BindOk deriving Show

instance Binary QueuePayload where
    put (Declare b c d e f g h) = putShort 10 >> putShort 0 >> put b >> (put (0 :: Word8)) >> put h 
    put (Bind b c d e f) =  putShort 20 >> putShort 0 >> put b >> put c >> put d >> put e >> put f

    get = get >>= getQueuePayload

getQueuePayload :: MethodId -> Get QueuePayload
getQueuePayload 11 = DeclareOk <$> get <*> get <*> get
getQueuePayload 21 = return BindOk