{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -fno-warn-orphans #-}

module Message (
    Datum
  , Message(..)
) where

import Control.Applicative
import Control.Monad (liftM2)
import Data.Aeson
import Data.Serialize
import Data.Word

import Constants

data Datum = DatumTypeNotImplemented Word8
           | CounterData Word32
           | TemperatureData Float
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
        | otherwise                   = return $ DatumTypeNotImplemented typeId

getCounterData = CounterData <$> getWord32le

getTemperatureData = TemperatureData <$> getFloat32le

instance ToJSON Datum where
  toJSON (DatumTypeNotImplemented typeId) =
    object [ "type_not_implemented" .= typeId ]

  toJSON (CounterData x) =
    object [ "counter" .= x ]

  toJSON (TemperatureData x) =
    object [ "temperature" .= x ]

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
    object [ "sync" .= sync, "data" .= ds ]

  toJSON _ = error "Unsupported toJSON (Message)"

