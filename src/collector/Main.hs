{-# LANGUAGE BangPatterns, GeneralizedNewtypeDeriving #-}

import Control.Applicative
import Control.Monad
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.Writer
import qualified Data.Aeson as A
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
        debug       = ccDebug cc
        device      = ccDevice cc
        -- use a modem inactivity timeout that's twice the sampling interval
        timeoutUsec = (fromIntegral $ ccInterval cc) * 2000000 

    -- Open modem and send initial ND command
    m <- openModem device debug
    outModem m atNDCommand

    -- Run the main loop forever: wait for decoder results or timeout
    forever $ timeout timeoutUsec (inModem m) >>= \r -> do
      x <- runCollector cc $ processDecoderResult r
      case x of
        -- Print error strings
        Left errStr        -> outError cc errStr
        -- Send response frames and stream JSON-formatted results, if any
        Right (ms, fs, ss) -> mapM_ (outDebug cc) ss >>
                              mapM_ (outModem m) fs >>
                              mapM_ outData ms
  where
    outData m     = BL.hPutStr IO.stdout (A.encode m) >> putStrLn ""
    outError cc x = when (ccDebug cc) $ IO.hPutStrLn IO.stderr $ "ERROR: " ++ x 
    outDebug cc x = when (ccDebug cc) $ IO.hPutStrLn IO.stderr $ "DEBUG: " ++ x 

-- Process DecoderResults received from the modem
processDecoderResult (Just (ReceivedFrame f)) =
  processFrame f

processDecoderResult (Just (DecoderError errStr)) =
  throwError $ "Decoder error: " ++ errStr

processDecoderResult Nothing =
  tellDebug ["Timed out without receiving any events from modem, resending neighbor discover (ATND)"] >> 
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

processMessage addr nwaddr m@(PollNotification sync _ _) = do
  -- Calculate probe clock drift: sync error occurs when drift > epsilon
  TOD now _ <- liftIO getClockTime
  cc <- ask
  let drift = fromIntegral now - sync
      epsilon = ccEpsilon cc
      syncErr = drift > fromIntegral epsilon

  -- When sync error occurs, resend probe request 
  when syncErr $
    tellDebug [printf "Probe %s drift is up to %u seconds, resending poll request" (show addr) drift] >>
    sendPollRequest addr nwaddr

  return [m]

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
  tellDebug ["Sending poll request to probe " ++ show addr]
  TOD now _ <- liftIO getClockTime
  cc <- ask
  let m = PollRequest
          { prSync = fromIntegral now
          , prSampleInterval = ccInterval cc
          , prProbeId = 1 }
      f = ZF.ZigBeeTransmitRequest 0 addr nwaddr 0 0 $ DS.encode m
  tellFrame [f]

atNDCommand = ZF.ATCommand 0 (ZF.commandName "ND") B.empty

-- Collector monad transformer stack
newtype Collector r = Collector { unCollector :: WriterT CollectorWriter (ErrorT String (ReaderT CollectorConfig IO)) r }
  deriving (Monad, MonadWriter CollectorWriter, MonadError String, MonadReader CollectorConfig, MonadIO)

-- Run an action in the Collector monad, returning either an error or
-- the result (including frames to be sent and debug strings)
runCollector :: CollectorConfig -> Collector r -> IO (Either String (r, [ZF.Frame], [String]))
runCollector cc act = do
  x <- runReaderT (runErrorT (runWriterT (unCollector act))) cc
  case x of
    Left errStr         -> return $ Left errStr
    Right (r, (fs, ss)) -> return $ Right (r, fs, ss)

-- Collector configuration: available through MonadReader
data CollectorConfig = CollectorConfig
  { ccDebug :: Bool
  , ccDevice :: FilePath
  , ccInterval :: Word8
  , ccEpsilon :: Word8
  }

-- Collector's writer type: supports writing frames as well as debug messages
-- (see also: http://stackoverflow.com/questions/7489968)
type CollectorWriter = ([ZF.Frame], [String])

tellFrame fs = tell (fs, mempty)
tellDebug ss = tell (mempty, ss)

throwDecoderError errStr = throwError $ "Decoder error: " ++ errStr

-- Command line argument processing
argsMode =
  initMode {
    modeNames = [ "collector" ]  
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
      ccDebug = isJust $ lookup flDebug args
    , ccDevice = device
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

