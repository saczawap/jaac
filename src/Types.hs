module Types where

import Data.Binary
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as E
import qualified Data.Text as T
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString
import Debug.Trace
import Data.Char

type Octet = Word8
type Bit = Bool
type NoWait = Bit
type NoLocal = Bit
type NoAck = Bit
type QueueName = ShortString
type MessageCount = Long
newtype ExchangeName = ExchangeName ShortString deriving(Show)
instance Binary ExchangeName where 
    get = ExchangeName <$> get
    put (ExchangeName name) = put name


type DeliveryTag = LongLong
type Redelivered = Bit

type ClassId = Word16
type MethodId = Word16

type FrameBinaryType = Word8
type ChannelSize = Word16
type Size = Word32
type LongStringStart = Word32
type ShortStringStart = Word8
type FieldValType = Word8
type Short = Word16
type Long = Word32
type LongLong = Word64
data FieldTable = FieldTable [(ShortString, FieldVal)] deriving(Show)

newtype LongString = LongString ByteString deriving(Show)
newtype ShortString = ShortString T.Text deriving(Show)
type ConsumerTag = ShortString
data FieldVal = 
    FVBool Bool 
    | FVFieldTable FieldTable 
    | FVShortString ShortString 
    | FVLongString LongString deriving(Show) 

instance Binary FieldVal where
    get = do
        fvType <- get :: (Get FieldValType)
        case chr (fromIntegral fvType) of
            't' -> FVBool <$> get
            's' -> FVShortString <$> get
            'F' -> FVFieldTable <$> get
            k -> error $ k : " error"
    put (FVBool bool) = do
        put 't'
        put bool 
    put (FVShortString str) = do 
        put 's'
        put str
    put (FVFieldTable ft) = do
        put 'F'
        put ft

readFields = readFields' [] 1 -- JUST ONE LEVEL FOR NOW
    
readFields' acc 0 = do
    return acc
readFields' acc n = do
    val <- get
    empty <- isEmpty
    if empty 
        then return (val:acc)
        else readFields' (val:acc) (n - 1)


putFields [] = put ()
putFields [(k, v)] = put k >> put v
putFields ((k, v) : rest) = put k >> put v >> putFields rest


instance Binary FieldTable where
    get = do
        LongString serverProperties <- get
        let l = runGet readFields (BL.fromStrict serverProperties)
        return $ FieldTable l
    put (FieldTable serverProperties) = do
        let p = runPut $ putFields serverProperties :: BL.ByteString
        putWord32be $ fromIntegral (BL.length p)
        putLazyByteString p

instance Binary LongString where
    get = do 
        length <- (get :: Get LongStringStart)
        string <- getByteString $ fromIntegral length
        return $ LongString string

    put (LongString b) = do
        putWord32be $ fromIntegral (BS.length b)
        putByteString b  

instance Binary ShortString where
    get = do
        length <- (get :: Get ShortStringStart)
        string <- getByteString $ fromIntegral length
        return $ ShortString $ E.decodeUtf8 string
    put (ShortString b) = do
        let s = E.encodeUtf8 b
        putWord8 $ fromIntegral (BS.length s)
        putByteString s

putShort x = put (x :: Short)

putEmptyShortString = put (0 :: Word8)