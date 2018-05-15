module Lib where

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString as NB
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString as BS
import Debug.Trace
import qualified Client as Client

import Data.Int (Int64)
import Data.Maybe
import Data.Text (Text)
import System.IO (hPutStrLn, stderr)

import Data.Word
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as BL
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except

someFunc :: IO ()
someFunc = do 
    conn <- Client.openConnection $ Client.ServerAddress "172.17.0.2" "5672"
    chann <- Client.openChannel conn
    ex <- Client.declareExchange chann "ename"
    q <- Client.declareQueue chann "nnn"
    bq <- Client.bindQueue chann "ename" "nnn"
    _ <- Client.publish conn "ename" "kkk" (BS8.pack "SOME")

    
    -- runExceptT $ consumeLoop sock
    putStrLn "DUPA"
    getLine
    putStrLn "END"

consumeLoop sock = (do
    Client.Message tag content <- Client.consume sock "nnn"
    putStrLn ("MSG: " ++ (show content))
    Client.ack sock tag) >> consumeLoop sock