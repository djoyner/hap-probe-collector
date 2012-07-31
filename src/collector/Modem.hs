{-# LANGUAGE BangPatterns #-}

module Modem (
    DecoderResult(..)
  , Modem
  , acquire
  , input
  , output
  , release
) where

import Control.Concurrent
import Control.Monad (when)
import Data.Conduit
import qualified Network.Protocol.ZigBee.ZNet25.Frame as ZF
import qualified System.IO as IO

import Conduit

data Modem =
  Modem
    { mHandle :: IO.Handle
    , mInChan :: Chan DecoderResult
    , mOutChan :: Chan ZF.Frame
    , mInThread :: ThreadId
    , mOutThread :: ThreadId
    , mDebug :: Bool }

acquire :: FilePath -> Bool -> IO Modem
acquire path debug = do
  h <- IO.openBinaryFile path IO.ReadWriteMode
  IO.hSetBuffering h IO.NoBuffering
  inChan <- newChan
  inThreadId <- forkIO $ runResourceT $ modemSource h $$ chanSink inChan
  outChan <- newChan
  outThreadId <- forkIO $ runResourceT $ chanSource outChan $$ modemSink h
  return $ Modem h inChan outChan inThreadId outThreadId debug

input :: Modem -> IO DecoderResult
input m = do
  r <- readChan $ mInChan m
  when (mDebug m) $ IO.hPutStrLn IO.stderr $ "IN: " ++ show r
  return r

output :: Modem -> ZF.Frame -> IO ()
output m !f = do
  when (mDebug m) $ IO.hPutStrLn IO.stderr $ "OUT: " ++ show f
  writeChan (mOutChan m) f

release :: Modem -> IO ()
release m = do
  killThread $ mInThread m
  killThread $ mOutThread m
  IO.hClose $ mHandle m

