{-# LANGUAGE OverloadedStrings #-}

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
           | WattHours Word32
           | AmbientTemp Word8
  deriving Show

instance Serialize Datum where
  put _ = error "Unsupported put (Datum)"

  get = do
      typeId <- getWord8
      _len <- getWord8
      go typeId
    where
      go typeId
        | typeId == dataIdWattHours   = getWattHours
        | typeId == dataIdAmbientTemp = getAmbientTemp
        | otherwise                   = return $ DatumTypeNotImplemented typeId

getWattHours = WattHours <$> getWord32le

getAmbientTemp = AmbientTemp <$> getWord8

instance ToJSON Datum where
  toJSON (DatumTypeNotImplemented typeId) =
    object [ "DatumTypeNotImplemented" .= typeId ]

  toJSON (WattHours x) =
    object [ "WattHours" .= x ]

  toJSON (AmbientTemp x) =
    object [ "AmbientTemp" .= x ]

data Message = MessageTypeNotImplemented Word8
             | PollRequest
               { prSync :: Word32
               , prSampleInterval :: Word8
               , prProbeId :: Word8 }
             | PollNotification
               { pnSync:: Word32
               , pnProbeId :: Word8
               , pnProbeData :: [Datum] }
  deriving Show

instance Serialize Message where
  put (PollRequest sync sampleInterval probeId) =
    putWord8 msgIdPollRequest >>
    putWord32le sync >>
    putWord8 sampleInterval >>
    putWord8 probeId
  
  put _ = error "Unsupported put (Message)"

  get = getWord8 >>= go 
    where
      go msgId
        | msgId == msgIdPollNotification = getPollNotification
        | otherwise                      = return $ MessageTypeNotImplemented msgId

getPollNotification =
  PollNotification <$>
    getWord32le <*>
    getWord8 <*>
    (getWord8 >> getProbeData)  -- skip count byte

getProbeData = isEmpty >>= go
  where
    go False = liftM2 (:) get getProbeData
    go True  = return []

instance ToJSON Message where
  toJSON (PollNotification sync probeId probeData) =
    object [ "sync" .= sync, "probeId" .= probeId, "probeData" .= probeData ]

  toJSON _ = error "Unsupported toJSON (Message)"

