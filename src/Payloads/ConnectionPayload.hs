{-# LANGUAGE OverloadedStrings #-}

module Payloads.ConnectionPayload where

import Data.Binary

import Types

data ConnectionPayload = 
    Start {
        versionMajor :: Octet,
        versionMinor :: Octet,
        serverProperties :: FieldTable,
        mechanisms :: LongString,
        locales :: LongString } | 
    StartOk {
        clientProperties :: FieldTable,
        mechanism :: ShortString,
        response :: LongString,
        locale :: ShortString } | 
    Tune {
        channelMax :: Short,
        frameMax :: Long,
        heartbeat :: Short } | 
    TuneOk {
        channelMax :: Short,
        frameMax :: Long,
        heartbeat :: Short } | 
    Open {
        virtualHost :: ShortString } | 
    OpenOk |
    Close {
        replyCode :: Short,
        replyText :: ShortString,
        classId :: ClassId,
        methodId :: MethodId
    } deriving Show


instance Binary ConnectionPayload where 
    get = get >>= getConnectionPayload

    put (StartOk a b c d) = putShort 11 >> put a >> put b >> put c >> put d
    put (TuneOk a b c ) =  putShort 31 >> put a >> put b >> put c
    put (Open a ) = putShort 40 >> put a >> putEmptyShortString >> put False


takeEmptyShort = do 
    0 <- getWord8
    return ()

getConnectionPayload :: MethodId -> Get ConnectionPayload
getConnectionPayload 10 = Start <$> getWord8 <*> getWord8 <*> get <*> get <*> get 
getConnectionPayload 30 = Tune <$> get <*> get <*> get
getConnectionPayload 41 = takeEmptyShort *> return OpenOk
getConnectionPayload 50 = Close <$> get <*> get <*> get <*> get 
