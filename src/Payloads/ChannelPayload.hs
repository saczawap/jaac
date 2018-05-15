module Payloads.ChannelPayload where

import Data.Binary

import Types

data ChannelPayload = 
    Open | 
    OpenOk
    deriving Show

instance Binary ChannelPayload where
    put Open = putShort 10 >> put (0 :: Octet) 
    get = get >>= getChannelPayload


getChannelPayload :: MethodId -> Get ChannelPayload
getChannelPayload 11 = (get :: Get LongString) >> return OpenOk
