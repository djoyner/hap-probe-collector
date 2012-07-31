module ZeroMQ (
    ZeroMQ
  , acquire
  , publish
  , release
) where

import qualified Data.ByteString as B
import System.ZMQ as Z

type ZeroMQ = (Context, Socket Pub)

acquire :: String -> IO ZeroMQ
acquire addr = do
  c <- Z.init 1
  s <- socket c Pub
  bind s addr
  return (c, s)

publish :: ZeroMQ -> B.ByteString -> IO ()
publish (_, s) bs = send s bs []

release :: ZeroMQ -> IO ()
release (c, s) = close s >> term c

