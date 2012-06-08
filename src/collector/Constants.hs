module Constants where

import Data.Word

-- Probe message id constants
msgIdPollRequest = 0x01 :: Word8
msgIdPollNotification = 0x02 :: Word8

-- Probe data id constants
dataIdWattHours = 0x01 :: Word8
dataIdAmbientTemp = 0x02 :: Word8

