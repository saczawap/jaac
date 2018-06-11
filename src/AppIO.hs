module AppIO where

import Connection
import Control.Monad.IO.Class

import Control.Concurrent
import qualified Data.Map.Strict as Map
import Frame
import Control.Monad


newtype AppIO a = AppIO { runIO :: IO a }

sendMsg2' :: Chan Frame -> Frame -> IO ()
sendMsg2' chan = writeChan chan

sendMsg2 :: Connection -> Frame -> IO ()
sendMsg2 (ConnectionHandler _ chan _) = sendMsg2' chan

sendMsg2'' :: Channel -> Frame -> IO ()
sendMsg2'' (ChannelHandler out inChan) = sendMsg2' out


receiveMsg2 :: Int -> Connection -> IO Frame
receiveMsg2 chn (ConnectionHandler _ _ mapa) = (do
    m <- readMVar mapa
    return $ m Map.! chn) >>= receiveMsg2'

receiveMsg2' :: Chan Frame -> IO Frame
receiveMsg2' chan = readChan chan

receiveMsg2'' :: Channel -> IO Frame
receiveMsg2'' (ChannelHandler out inChan) = receiveMsg2' inChan


instance SendMessage AppIO where
    sendMessage conn frame = AppIO (sendMsg2 conn frame)

instance ReceiveMessage AppIO where
    receiveMessage a b = AppIO (receiveMsg2 a b)


instance ModifyChannelMap AppIO where
    takeChannelNumber (ConnectionHandler mvar _ _) = AppIO (modifyMVar mvar (\val -> return ((val + 1), val)) )

    addChannelChan (ConnectionHandler _ _ mapa) n = AppIO $ do
        newChan <- newChan :: (IO (Chan Frame))
        _ <- modifyMVar_ mapa (\m -> return (Map.insert n newChan m))
        return newChan

kii :: AppIO a -> IO a
kii (AppIO a) = a

fii :: IO (AppIO a) -> IO a
fii x = join (fmap kii x)        

instance Monad AppIO where
    (AppIO x) >>= f = AppIO ( fii (fmap f x) )

instance MonadIO AppIO where
    liftIO x = AppIO x


instance Applicative AppIO where
    pure x = AppIO (pure x)
    (AppIO f) <*> (AppIO x) = AppIO (f <*> x)

instance Functor AppIO where
    fmap f (AppIO x) = AppIO (fmap f x)

