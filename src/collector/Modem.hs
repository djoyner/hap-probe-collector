{-# LANGUAGE BangPatterns #-}

module Modem (
    DecoderResult(..)
  , Modem
  , openModem
  , inModem
  , outModem
) where

import Control.Concurrent
import Control.Monad (when)
import Data.Conduit
import qualified Network.Protocol.ZigBee.ZNet25.Frame as ZF
import qualified System.IO as IO

import Conduit

-- Modem is an I/O channel pair
data Modem = Modem
             { mInChan :: Chan DecoderResult
             , mOutChan :: Chan ZF.Frame
             , mDebug :: Bool }

-- Open modem
openModem :: FilePath -> Bool -> IO (Modem)
openModem path debug = do
    h <- IO.openBinaryFile path IO.ReadWriteMode
    IO.hSetBuffering h IO.NoBuffering
    inChan <- newChan
    _ <- forkIO $ runResourceT $ modemSource h $$ chanSink inChan
    outChan <- newChan
    _ <- forkIO $ runResourceT $ chanSource outChan $$ modemSink h
    return $ Modem inChan outChan debug

-- Modem input/output
inModem :: Modem -> IO DecoderResult
inModem m = do
  r <- readChan $ mInChan m
  when (mDebug m) $ IO.hPutStrLn IO.stderr $ "IN: " ++ show r
  return r

outModem :: Modem -> ZF.Frame -> IO ()
outModem m !f = do
  when (mDebug m) $ IO.hPutStrLn IO.stderr $ "OUT: " ++ show f
  writeChan (mOutChan m) f

