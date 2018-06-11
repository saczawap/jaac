module Client where
{-# LANGUAGE OverloadedStrings #-}
{-# InstanceSigs #-}

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString as BS
import Debug.Trace
import Network.Socket.ByteString as NB
import Data.Int (Int64)
import Data.Maybe
import System.IO (hPutStrLn, stderr)
import Payloads.Payload
import qualified Data.Binary as DB
import Data.Version(showVersion)
import Types

import qualified Payloads.BasicPayload as BasicPayload
import qualified Payloads.ConnectionPayload as ConnectionPayload
import qualified Payloads.ChannelPayload as ChannelPayload
import qualified Payloads.ExchangePayload as ExchangePayload
import qualified Payloads.HeaderPayload as HeaderPayload
import qualified Payloads.QueuePayload as QueuePayload
import qualified Payloads.ContentPayload as ContentPayload
import qualified Data.Map.Strict as Map

import qualified Data.Text.Encoding as E
import qualified Data.Text as T
import Frame

import Data.Binary.Get
import Data.Word
import qualified Data.ByteString.Lazy as BL
import Control.Monad.Trans.Except
import Data.Bifunctor
import Control.Monad.Trans.Class
import Control.Monad
import Control.Concurrent
import Control.Monad.IO.Class

import Connection


getFrameSize = do
    _ <- getWord8
    _ <- DB.get :: (DB.Get Word16)
    a <- DB.get :: (DB.Get Word32)
    return a

getSize :: BL.ByteString -> Integer
getSize bs = fromIntegral $ (runGet getFrameSize) bs

getBytess :: Socket -> IO BS.ByteString
getBytess sock = do
    head <- NB.recv sock 7
    let size = getSize $ BL.fromStrict head
    tail <- NB.recv sock (fromInteger $ size + 1)
    return $ BS.append head tail

third (a, b, c) = c

readFrame :: Socket -> IO (Either String Frame)
readFrame sock = do
    bytes <- getBytess sock
    let x = decode $ BL.fromStrict bytes
    return $ bimap third third x

openTcpConnection :: ServerAddress -> IO Socket
openTcpConnection (ServerAddress address port) = do
    addrInfo <- getAddrInfo Nothing (Just address) (Just port)
    let serverAddr = head addrInfo
    sock <- socket (addrFamily serverAddr) Stream defaultProtocol
    connect sock (addrAddress serverAddr)
    NB.send sock $ BS8.append (BS8.pack "AMQP") (BS.pack [0, 0, 9, 1])
    return sock


decode = runGetOrFail (DB.get :: Get Frame)

sendLoop :: Socket -> Chan Frame -> IO ()
sendLoop socket chan = forever( do
    frame <- readChan chan
    NB.send socket (BL.toStrict $ DB.encode $ frame))

handleFrames :: BL.ByteString -> MM -> IO ()
handleFrames byteString mapa = do
    let (Right oi) = decode byteString
    let (rest, _, frame) = oi
    m <- readMVar mapa
    let chanNumber = channel frame
    let ch = m Map.! (fromInteger (toInteger chanNumber))
    writeChan ch frame
    if (BL.length rest > 0) then (handleFrames rest mapa) else return ()


receiveLoop :: Socket -> MM -> IO ()
receiveLoop socket mapa = forever (do
    bytes <- NB.recv socket 8192
    handleFrames (BL.fromStrict bytes) mapa)
    

openConnection' :: ServerAddress -> Chan Frame -> MM -> IO ()
openConnection' address ch mapa = do
    sock <- openTcpConnection address
    _ <- readFrame sock
    NB.send sock startOk
    _ <- readFrame sock
    NB.send sock tuneOk
    NB.send sock open
    _ <- readFrame sock
    forkIO (sendLoop sock ch)
    receiveLoop sock mapa

sendMsg' :: Chan Frame -> Frame -> IO ()
sendMsg' chan = writeChan chan

sendMsg :: Connection -> Frame -> IO ()
sendMsg (ConnectionHandler _ chan _) = sendMsg' chan

sendMsg'' :: Channel -> Frame -> IO ()
sendMsg'' (ChannelHandler out inChan) = sendMsg' out

openConnection :: ServerAddress -> IO Connection
openConnection address = do
    channels <- newMVar 1
    channel <- newChan
    channelMap <- newMVar Map.empty
    _ <- forkIO (openConnection' address channel channelMap)
    return $ ConnectionHandler channels channel channelMap


open = BL.toStrict $ DB.encode $ getConnectionFrame $ ConnectionPayload.Open (ss "/") 
tuneOk = BL.toStrict $ DB.encode $ getConnectionFrame $ ConnectionPayload.TuneOk 2047 131072 60

startOk = BL.toStrict $ DB.encode $ getConnectionFrame $ ConnectionPayload.StartOk (FieldTable []) (ss "PLAIN") (LongString plain) (ss "en_US")

ss = ShortString . T.pack

plain :: BS.ByteString
plain = E.encodeUtf8 $ (T.cons nul (T.pack "guest")) `T.append` (T.cons nul (T.pack "guest"))
  where
    nul = '\0'




receiveMsg :: Int -> Connection -> IO Frame
receiveMsg chn (ConnectionHandler _ _ mapa) = (do
    m <- readMVar mapa
    return $ m Map.! chn) >>= receiveMsg'

receiveMsg' :: Chan Frame -> IO Frame
receiveMsg' chan = readChan chan

receiveMsg'' :: Channel -> IO Frame
receiveMsg'' (ChannelHandler out inChan) = receiveMsg' inChan





openChannel :: (MonadIO m, SendMessage m, ReceiveMessage m, ModifyChannelMap m) => Connection -> m Channel
openChannel connection = do
    chanNum <- takeChannelNumber connection
    inChan <- addChannelChan connection chanNum
    sendMessage connection (openCh (fromIntegral chanNum))
    f <- receiveMessage chanNum connection
    let ConnectionHandler _ out _ = connection 
    return $ ChannelHandler out inChan

openCh chanNum = getChannelFrame (ChannelPayload.Open) chanNum

declareExchange :: Channel -> String -> IO ()
declareExchange (ChannelHandler out inChan) name = do 
    sendMsg' out (de name)
    frame <- receiveMsg' inChan
    putStrLn (show frame)
    return ()

de name = getExchangeFrame $ ExchangePayload.Declare (ExchangeName $ ss name) (ss "direct") False True False (FieldTable [])

declareQueue :: Channel -> String -> IO Queue 
declareQueue (ChannelHandler out inChan) name = do
    sendMsg' out (dq name)
    frame <- receiveMsg' inChan
    putStrLn (show frame)
    return QueueHandler

dq name = getQueueFrame $ QueuePayload.Declare (ss name) False False False False False (FieldTable [])

bindQueue :: Channel -> String -> String -> IO ()
bindQueue channel exName qName = do
    sendMsg'' channel (bq exName qName)
    frame <- receiveMsg'' channel
    putStrLn (show frame)

bq exName qName = getQueueFrame $ QueuePayload.Bind (ss qName) (ExchangeName $ ss exName) (ss "routingKey") False (FieldTable []) 

publish :: Connection -> String -> String -> BS.ByteString -> IO ()
publish connection exName routingKey str = do
    _ <- sendMsg connection (p exName routingKey) 
    -- bytes <- NB.recv sock 8192
    -- putStrLn (show $ decode $ BL.fromStrict bytes)

    _ <- sendMsg connection (hp str)

    _ <- sendMsg connection  (pp str)

    return ()

p exName routingKey = getBasicFrame $ BasicPayload.Publish (ExchangeName (ss exName)) (ss routingKey) False False

hp str = getHeaderFrame $ HeaderPayload.HP ((fromIntegral $ BL.length (BL.fromStrict str) :: LongLong))
pp str = getContentFrame $ ContentPayload.CP $ BL.fromStrict str

pullFrame :: Connection -> IO Frame
pullFrame connection = receiveMsg 1 connection

    
startConsuming :: Connection -> String -> IO ()
startConsuming connection qName = do 
    let h = (getBasicFrame $ BasicPayload.Consume (ss qName) (ss "abc") False False False False (FieldTable []))
    _ <- sendMsg connection h
    consumeOk <- pullFrame connection
    return ()

consume :: Connection -> String -> IO Message
consume connection qName = do 
    frame <- pullFrame connection
    putStrLn $ show frame
    let ConnectionFrame _ (BasicPayload (BasicPayload.Deliver a b c d e)) = frame
    f <- pullFrame connection
    let HeaderFrame _ _ = f
    next <- pullFrame connection
    let ContentFrame _ (ContentPayload.CP(content)) = next
    return $ Message b content    

data Message = Message DeliveryTag BL.ByteString deriving Show

ack :: Connection -> DeliveryTag -> IO ()
ack connection tag = do
    _ <- sendMsg connection $ getBasicFrame $ BasicPayload.Ack tag False 
    return ()