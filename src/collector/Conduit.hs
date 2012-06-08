{-# LANGUAGE BangPatterns, CPP #-}

module Conduit (
    DecoderResult(..)
  , chanSource
  , chanSink
  , modemSource
  , modemSink
) where

import Control.Concurrent.Chan
import Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.State as S
import qualified Data.ByteString as B 
import Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Network.Protocol.ZigBee.ZNet25 as Z
import qualified System.IO as IO
#ifdef DEBUG
import Text.Printf
#endif

-- Stream from a Chan
chanSource :: (MonadResource m) => Chan a -> Source m a
chanSource c = sourceIO
  (return ())
  (const $ return ())
  (const $ (liftIO . readChan) c >>= (return . IOOpen))

-- Stream to a Chan
chanSink :: (MonadResource m) => Chan a -> Sink a m ()
chanSink c = sinkIO
  (return ())
  (const $ return ())
  (\_ !x -> (liftIO . writeChan c) x >> return IOProcessing)
  (const $ return ())

-- Stream from a modem Handle, deframing/decoding and producing DecoderResults
modemSource :: (MonadResource m) => IO.Handle -> Source m DecoderResult
modemSource h = CB.sourceHandle h $=
#ifdef DEBUG
                hexdump "RX: " =$=
#endif
                deframer

data DecoderResult = ReceivedFrame Z.Frame
                   | DecoderError String
  deriving Show

deframer :: Monad m => Conduit B.ByteString m DecoderResult
deframer = conduitState Z.initDecode push close
  where
    push ds bs = return $ StateProducing ds' $ map unwrap rs 
      where
        unwrap (Left err) = DecoderError err
        unwrap (Right f)  = ReceivedFrame f
        (rs, ds')         = S.runState (Z.decode bs) ds
    close _  = return []

-- Stream Frames to a modem Handle
modemSink :: (MonadResource m) => IO.Handle -> Sink Z.Frame m ()
modemSink h = framer =$= 
#ifdef DEBUG
              hexdump "TX: " =$
#endif
              CB.sinkHandle h

framer :: Monad m => Conduit Z.Frame m B.ByteString
framer = conduitState () push close
  where
    push _ f = return $ StateProducing () $ Z.encode f
    close _  = return []

#ifdef DEBUG
-- Render 'ByteString' as an ASCII hex dump
hexdump :: MonadResource m => String -> Conduit B.ByteString m B.ByteString
hexdump label = conduitIO
  (return ())
  (const $ return ())
  (\_ x -> do
    liftIO $ putStr label
    mapM_ (liftIO . putStr . printf "%02x ") $ B.unpack x
    liftIO $ putStrLn ""
    return $ IOProducing [x])
  (const $ return [])
#endif

