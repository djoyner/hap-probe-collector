{-# LANGUAGE BangPatterns, GeneralizedNewtypeDeriving, OverloadedStrings #-}
{-# OPTIONS -fno-warn-orphans #-}

module Main (main) where

import Control.Applicative
import Control.Monad
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Maybe
import qualified Data.Serialize as DS
import Data.Word
import qualified Network.Protocol.ZigBee.ZNet25.Frame as ZF
import System.Console.CmdArgs.Explicit
import System.Exit
import qualified System.IO as IO
import System.Time
import System.Timeout
import Text.Printf

import Message
import Modem

defaultInterval = "15" -- seconds
defaultEpsilon  = "1"  -- seconds

main = do
    -- Process command line arguments
    !args <- processArgs argsMode
    when (isJust $ lookup flHelp args) $ do
      print $ helpText [] HelpFormatDefault argsMode
      exitSuccess

    let cc          = configFromArgs args
        debug       = isJust $ lookup flDebug args
        device      = ccDevice cc
        -- use a modem inactivity timeout that's twice the sampling interval
        timeoutUsec = (fromIntegral $ ccInterval cc) * 2000000 

    IO.hSetBuffering IO.stdout IO.LineBuffering
    IO.hSetBuffering IO.stderr IO.LineBuffering

    -- Open modem and send initial ND command
    outLog $ "Opening modem " ++ device ++ " and sending neighbor discovery command (ATND)"
    m <- openModem device debug
    outModem m atNDCommand

    -- Run the main loop forever: wait for decoder results or timeout
    forever $ timeout timeoutUsec (inModem m) >>= \r -> do
      x <- runCollector cc $ processDecoderResult r
      case x of
        -- Print error strings
        Left errStr        -> outError errStr
        -- Send response frames and stream JSON-formatted results, if any
        Right (ds, fs, ss) -> mapM_ outLog ss >>
                              mapM_ (outModem m) fs >>
                              mapM_ outJSON ds
  where
    outLog            = IO.hPutStrLn IO.stderr
    outError x        = IO.hPutStrLn IO.stderr $ "ERROR: " ++ x 
    outJSON (addr, m) = BL.hPutStr IO.stdout (encode $ wrap addr m) >> IO.hPutStrLn IO.stdout ""
    wrap addr m       = object [ "type" .= ("probe" :: String), "address " .= show addr, "message" .= m ]

-- Process DecoderResults received from the modem
processDecoderResult (Just (ReceivedFrame f)) =
  processFrame f

processDecoderResult (Just (DecoderError errStr)) =
  throwError errStr

processDecoderResult Nothing =
  tellLog ["Timed out without receiving any events from modem, resending neighbor discover (ATND)"] >> 
  tellFrame [atNDCommand] >>
  return []

-- Process specific frame types
processFrame (ZF.ATCommandResponse _frameId cmdName _cmdStatus val) =
  processATResponse (ZF.unCommandName cmdName) val

processFrame (ZF.NodeIdentificationIndicator addr nwaddr _ _ _ _ _ _ _ _ _) =
  sendPollRequest addr nwaddr >>
  return []

processFrame (ZF.ZigBeeReceivePacket addr nwaddr _ val) =
  case DS.decode val :: Either String Message of
    Left errStr -> throwDecoderError errStr
    Right m -> processMessage addr nwaddr m

processFrame _ = return []

-- Process poll notifications from the probe
processMessage addr nwaddr m@(PollNotification sync _) = do
  -- Calculate probe clock drift: sync error occurs when drift > epsilon
  TOD now _ <- liftIO getClockTime
  cc <- ask
  let drift = fromIntegral now - sync
      epsilon = ccEpsilon cc
      syncErr = drift > fromIntegral epsilon

  -- When sync error occurs, resend probe request 
  when syncErr $
    tellLog [printf "Probe %s drift is up to %u seconds, resending poll request" (show addr) drift] >>
    sendPollRequest addr nwaddr

  return [(addr, m)]

processMessage _ _ _ = return []

-- Process AT command results
data ATNDResponse = ATNDResponse ZF.NetworkAddress ZF.Address

processATResponse "ND" val =
  case DS.runGet (ATNDResponse <$> DS.get <*> DS.get) val of
    Left errStr -> throwDecoderError errStr
    Right (ATNDResponse nwaddr addr) -> sendPollRequest addr nwaddr >> return []

processATResponse _ _ = return []

-- Send an application-level poll request message to a probe
sendPollRequest addr nwaddr = do
  tellLog ["Sending poll request to probe " ++ show addr]
  TOD now _ <- liftIO getClockTime
  cc <- ask
  let m = PollRequest
          { prSync = fromIntegral now
          , prSampleInterval = ccInterval cc }
      f = ZF.ZigBeeTransmitRequest 0 addr nwaddr 0 0 $ DS.encode m
  tellFrame [f]

atNDCommand = ZF.ATCommand 0 (ZF.commandName "ND") B.empty

-- Collector monad transformer stack
newtype Collector r = Collector { unCollector :: WriterT CollectorWriter (ErrorT String (ReaderT CollectorConfig IO)) r }
  deriving (Monad, MonadWriter CollectorWriter, MonadError String, MonadReader CollectorConfig, MonadIO)

-- Run an action in the Collector monad, returning either an error or
-- the result (including frames to be sent and log strings)
runCollector :: CollectorConfig -> Collector r -> IO (Either String (r, [ZF.Frame], [String]))
runCollector cc act = do
  x <- runReaderT (runErrorT (runWriterT (unCollector act))) cc
  case x of
    Left errStr         -> return $ Left errStr
    Right (r, (fs, ss)) -> return $ Right (r, fs, ss)

-- Collector configuration: available through MonadReader
data CollectorConfig = CollectorConfig
  { ccDevice :: FilePath
  , ccInterval :: Word8
  , ccEpsilon :: Word8
  }

-- Collector's writer type: supports writing frames as well as log messages
-- (see also: http://stackoverflow.com/questions/7489968)
type CollectorWriter = ([ZF.Frame], [String])

tellFrame fs = tell (fs, mempty)
tellLog ss   = tell (mempty, ss)

throwDecoderError errStr = throwError $ "Decoder error: " ++ errStr

-- JSON instances
instance ToJSON ZF.Address where
  toJSON = toJSON . ZF.unAddress

-- Command line argument processing
argsMode =
  initMode {
    modeNames = [ "hap-probe-collector" ]  
  , modeHelp = "Home Automation Project: probe collector"
  , modeGroupFlags = toGroup [ 
      flagNone [flDebug, "D"] (\v -> (flDebug, ""):v) "Enable debug output"
    , flagReq [flDevice, "d"] (updateArg flDevice) "DEVICE" "Path to modem device"
    , flagReq [flInterval, "i"] (updateArg flInterval) "SECS" $ printf "Sampling interval (default: %s sec)" defaultInterval
    , flagReq [flEpsilon, "e"] (updateArg flEpsilon) "SECS" $ printf "Time sync error tolerance (default: %s sec)" defaultEpsilon
    , flagHelpSimple ((flHelp, ""):)
    ]
  }
  where
    updateArg fl x v = Right $ (fl, x):v
    initMode         = modeEmpty [
                          (flInterval, defaultInterval)
                        , (flEpsilon, defaultEpsilon)
                        ]

configFromArgs args =
    CollectorConfig {
      ccDevice = device
    , ccInterval = interval
    , ccEpsilon = epsilon
    }
  where
    device   = case lookup flDevice args of
                 Just dev -> dev
                 Nothing  -> error "Missing device (-d)"
    interval = case (maybeRead . fromJust . lookup flInterval) args of
                 Just int -> int
                 Nothing  -> error "Invalid interval"
    epsilon  = case (maybeRead . fromJust . lookup flEpsilon) args of
                 Just int -> int
                 Nothing  -> error "Invalid epsilon"

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

flDebug    = "debug"
flDevice   = "device"
flInterval = "interval"
flEpsilon  = "epsilon"
flHelp     = "help"

