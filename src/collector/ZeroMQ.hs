module ZeroMQ (
    ZeroMQ
  , acquire
  , publish
  , release
) where

import Control.Monad (when)
import qualified Data.ByteString as B
import qualified System.IO as IO
import System.ZMQ as Z

type ZeroMQ = (Context, Socket Pub, Bool)

acquire :: String -> Bool -> IO ZeroMQ
acquire addr debug = do
  when debug $ IO.hPutStrLn IO.stderr $ "Acquiring ZeroMQ PUB socket " ++ addr
  c <- Z.init 1
  s <- socket c Pub
  bind s addr
  return (c, s, debug)

publish :: ZeroMQ -> B.ByteString -> IO ()
publish (_, s, debug) bs = do
  when debug $ do
    IO.hPutStr IO.stderr "PUB: "
    B.hPutStr IO.stderr bs
    IO.hPutStrLn IO.stderr ""
  send s bs []

release :: ZeroMQ -> IO ()
release (c, s, debug) = do
  when debug $ IO.hPutStrLn IO.stderr $ "Releasing ZeroMQ PUB socket"
  close s
  term c

