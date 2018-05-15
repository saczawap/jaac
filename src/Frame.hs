module Frame where

import Data.Binary
import Data.Binary.Put

import qualified Data.ByteString.Lazy as BL

import Payloads.BasicPayload
import Payloads.ChannelPayload
import Payloads.ConnectionPayload
import Payloads.ExchangePayload
import Payloads.HeaderPayload
import Payloads.Payload
import Payloads.QueuePayload
import Payloads.ContentPayload
import Types
import Debug.Trace
import Data.Binary.Get

data Frame = 
    ConnectionFrame {    
        channel :: ChannelSize,    
        payload :: Payload } |
    HeaderFrame {    
        channel :: ChannelSize,    
        headerPayload :: HeaderPayload } | 
    ContentFrame {    
        channel :: ChannelSize,
        contentPayload :: ContentPayload } deriving Show

putFrameType :: Frame -> Put
putFrameType (ConnectionFrame _ _) = put (1 :: FrameBinaryType)
putFrameType (HeaderFrame _ _) = put (2 :: FrameBinaryType)
putFrameType (ContentFrame _ _) = put (3 :: FrameBinaryType)


ggetFrame :: FrameBinaryType -> Get Frame
ggetFrame 1 = do 
    ch <- get
    size <- (get :: Get Size)
    payload <- get
    return $ ConnectionFrame ch payload 
ggetFrame 2 = do 
    ch <- get
    size <- (get :: Get Size)
    payload <- get
    return $ HeaderFrame ch payload 
ggetFrame 3 = do 
    ch <- (get :: Get ChannelSize)
    payload <- get
    return $ ContentFrame ch payload 
ggetFrame z = trace ("ZZZ gget frame " ++ (show z)) (do 
    ch <- get
    size <- (get :: Get Size)
    payload <- get
    return $ HeaderFrame ch payload )

kkk :: Frame -> BL.ByteString -> Put
kkk frame buf = do
    putFrameType frame
    put (channel frame)
    -- let buf = runPut pp -- $ put (payload frame)
    put ((fromIntegral $ BL.length buf)::Size)
    putLazyByteString buf
    putWord8 0xCE

takeFrameEnd frame = do
    0xCE <- getWord8

    return frame
    
instance Binary Frame where
    put frame@(ConnectionFrame chanell ppp) = kkk frame (runPut $ put (ppp))
    put frame@(HeaderFrame chanell ppp) = kkk frame (runPut $ put (ppp))
    put frame@(ContentFrame chanell ppp) = kkk frame (runPut $ put (ppp))

    -- put frame = do
    --     putFrameType frame
    --     put (channel frame)
    --     let buf = runPut $ put (payload frame)
    --     put ((fromIntegral $ BL.length buf)::Size)
    --     putLazyByteString buf
    --     putWord8 0xCE
    get = get >>= ggetFrame >>= takeFrameEnd
        -- frameType <- get
        -- ch <- get
        -- size <- (get :: Get Size)
        -- payload <- get
        -- return $ ConnectionFrame ch payload 

getConnectionFrame :: ConnectionPayload -> Frame
getConnectionFrame payload = ConnectionFrame 0 $ ConnectionPayload payload

getChannelFrame :: ChannelPayload -> ChannelSize -> Frame
getChannelFrame payload channel = ConnectionFrame channel $ ChannelPayload payload

getExchangeFrame :: ExchangePayload -> Frame
getExchangeFrame payload = ConnectionFrame 1 $ ExchangePayload payload

getQueueFrame :: QueuePayload -> Frame
getQueueFrame payload = ConnectionFrame 1 $ QueuePayload payload

getBasicFrame :: BasicPayload -> Frame
getBasicFrame payload = ConnectionFrame 1 $ BasicPayload payload

getHeaderFrame :: HeaderPayload -> Frame
getHeaderFrame payload = HeaderFrame 1 payload

getContentFrame :: ContentPayload -> Frame
getContentFrame payload = ContentFrame 1 payload
