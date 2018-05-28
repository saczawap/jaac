module Lib where

import qualified Data.ByteString.Char8 as BS8
import qualified Client as Client

example :: IO ()
example = do 
    connection <- Client.openConnection serverAddress
    channel <- Client.openChannel connection 
    _ <- Client.declareExchange channel exchangeName
    queue <- Client.declareQueue channel queueName
    _ <- Client.bindQueue channel exchangeName queueName
    _ <- Client.publish connection exchangeName routingKey (BS8.pack "A MESSAGE")
    _ <- putStrLn "Starting consuming messages"
    consume connection

consume sock = do
    _ <- Client.startConsuming sock queueName
    _ <- consumeLoop sock
    return ()

consumeLoop sock = (do
    Client.Message tag content <- Client.consume sock queueName
    putStrLn ("MSG: " ++ (show content))
    Client.ack sock tag) >> consumeLoop sock


exchangeName = "exchangeName"
queueName = "queueName"
routingKey = "routingKey"

serverAddress = Client.ServerAddress "172.17.0.2" "5672"