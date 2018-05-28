module Payloads.BasicPayload where

import Data.Binary

import Types
import Debug.Trace

data BasicPayload = 
    Publish {
        exchane :: ExchangeName,
        routingKey :: ShortString,
        mandatory :: Bit,
        immediate :: Bit } |
    Consume {
        queueName :: QueueName,
        consumerTag :: ConsumerTag,
        noLocal :: NoLocal,
        noAck :: NoAck,
        exclusive :: Bit,
        noWait :: NoWait,
        arguments :: FieldTable 
    } | ConsumeOk {
        consumerTag :: ConsumerTag
    } | Deliver {
        consumerTag :: ConsumerTag,
        deliveryTag :: DeliveryTag,
        redelivered :: Redelivered,
        exchange :: ExchangeName,
        routingKey :: ShortString
    } | Ack {
        deliveryTag :: DeliveryTag,
        multiple :: Bit
    } deriving Show


instance Binary BasicPayload where 
    get = get >>= getBasicPayload
    put (Publish a b c d) = putShort 40 >> putShort 0 >> put a >> put b >> put (0 :: Word8)
    put (Consume a b c d e f g) = putShort 20 >> putShort 0 >> put a >> put b >> put (0 :: Word8) >> put g
    put (Ack a b) = putShort 80 >> put a >> put b

getBasicPayload :: ClassId -> Get BasicPayload
getBasicPayload 21 = ConsumeOk <$> get
getBasicPayload 60 = Deliver <$> get <*> get <*> get <*> get <*> get

getBasicPayload z = trace ("ZZZ:" ++ (show z)) (ConsumeOk <$> get)

