{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -fno-warn-orphans #-}

module Message (
    Datum
  , Message(..)
) where

import Control.Applicative
import Control.Monad (liftM2)
import Data.Aeson
import qualified Data.HashMap.Strict as HashMap
import Data.Serialize
import Data.Word

import Constants

data Datum = DatumTypeNotImplemented Word8
           | CounterData Word32
           | TemperatureData Float
           | RelHumidityData Float
  deriving Show

instance Serialize Datum where
  put _ = error "Unsupported put (Datum)"

  get = do
      typeId <- getWord8
      _len <- getWord8
      go typeId
    where
      go typeId
        | typeId == dataIdCounter     = getCounterData
        | typeId == dataIdTemperature = getTemperatureData
        | typeId == dataIdRelHumidity = getRelHumidityData
        | otherwise                   = return $ DatumTypeNotImplemented typeId

getCounterData = CounterData <$> getWord32le

getTemperatureData = TemperatureData <$> getFloat32le

getRelHumidityData = RelHumidityData <$> getFloat32le

toPair (DatumTypeNotImplemented typeId) = "type_not_implemented" .= typeId
toPair (CounterData x)                  = "counter" .= x
toPair (TemperatureData x)              = "temperature" .= x
toPair (RelHumidityData x)              = "rel_humidity" .= x

data Message = MessageTypeNotImplemented Word8
             | PollRequest
               { prSync :: Word32
               , prSampleInterval :: Word8 }
             | PollNotification
               { pnSync:: Word32
               , pnData :: [Datum] }
  deriving Show

instance Serialize Message where
  put (PollRequest sync sampleInterval) =
    put msgIdPollRequest >>
    putWord32le sync >>
    put sampleInterval
  
  put _ = error "Unsupported put (Message)"

  get = getWord8 >>= go 
    where
      go msgId
        | msgId == msgIdPollNotification = getPollNotification
        | otherwise                      = return $ MessageTypeNotImplemented msgId

getPollNotification =
  PollNotification <$>
    getWord32le <*>
    (getWord8 >> getDataList)  -- skip count byte

getDataList = isEmpty >>= go
  where
    go False = liftM2 (:) get getDataList
    go True  = return []

instance ToJSON Message where
  toJSON (PollNotification sync ds) =
    object [ "sync" .= sync, ("data", Object $ HashMap.fromList $ map toPair ds) ]

  toJSON _ = error "Unsupported toJSON (Message)"

