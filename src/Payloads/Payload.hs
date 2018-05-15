module Payloads.Payload where

import Data.Binary

import Payloads.BasicPayload
import Payloads.ChannelPayload
import Payloads.ConnectionPayload
import Payloads.ExchangePayload
import Payloads.QueuePayload
import Types

import Debug.Trace


data Payload = 
    ConnectionPayload ConnectionPayload |
    ChannelPayload ChannelPayload |
    ExchangePayload ExchangePayload |
    QueuePayload QueuePayload |
    BasicPayload BasicPayload
    deriving Show

instance Binary Payload where
    put (ConnectionPayload payload) = putShort 10 >> put payload
    put (ChannelPayload payload) = putShort 20 >> put payload
    put (ExchangePayload payload) = putShort 40 >> put payload
    put (QueuePayload payload) = putShort 50 >> put payload
    put (BasicPayload payload) = putShort 60 >> put payload
    get = get >>= getPayload

getPayload :: ClassId -> Get Payload
getPayload 10 = ConnectionPayload <$> get
getPayload 20 = ChannelPayload <$> get
getPayload 40 = ExchangePayload <$> get
getPayload 50 = QueuePayload <$> get
getPayload 60 = BasicPayload <$> get