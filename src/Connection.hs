module Connection where

import Control.Concurrent
import Frame
import qualified Data.Map.Strict as Map

data Connection = ConnectionHandler (MVar Int) (Chan Frame) (MM)
type MM = (MVar (Map.Map Int (Chan Frame)))

data ServerAddress = ServerAddress {address :: String, port :: String}
data Channel = ChannelHandler (Chan Frame) (Chan Frame)
data Queue = QueueHandler

class SendMessage m where
    sendMessage :: Connection -> Frame -> m ()

class ReceiveMessage m where
    receiveMessage :: Int -> Connection -> m Frame

class ModifyChannelMap m where
    takeChannelNumber :: Connection -> m Int

    addChannelChan :: Connection -> Int -> m (Chan Frame)
