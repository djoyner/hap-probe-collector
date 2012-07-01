module Constants where

import Data.Word

-- Probe message id constants
msgIdPollRequest = 0x01 :: Word8
msgIdPollNotification = 0x02 :: Word8

-- Probe data id constants
dataIdCounter = 0x01 :: Word8
dataIdTemperature = 0x02 :: Word8
dataIdRelHumidity = 0x03 :: Word8

