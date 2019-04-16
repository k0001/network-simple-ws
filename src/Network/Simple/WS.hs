{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Simple tools for establishing and using insecure WebSockets connections on top of
-- TCP (i.e, @ws:\/\/@).
--
-- See the
-- [network-simple-wss](https://hackage.haskell.org/package/network-simple-wss)
-- package for Secure WebSockets (i.e, @wss:\/\/@) support.
--
-- Notice that, currently, this is package offers tools that are mostly
-- intreresting from a client's point of view. Server side support will come
-- later.
module Network.Simple.WS
 ( W.Connection
 , send
 , recv
   -- * Client side
 , connect
 , connectSOCKS5
   -- * Low level
 , clientConnectionFromStream
 , streamFromSocket
 ) where


import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Control.Exception.Safe as Ex
import Data.Bifunctor (first)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.CaseInsensitive as CI
import Data.Foldable (traverse_)
import Data.Function (fix)

import qualified Network.Simple.TCP as T
import qualified Network.WebSockets as W
import qualified Network.WebSockets.Stream as W (Stream, makeStream, close)

--------------------------------------------------------------------------------

-- | Connect to the specified WebSockets server.
connect
  :: (MonadIO m, Ex.MonadMask m)
  => T.HostName
  -- ^ WebSockets server host name (e.g., @\"www.example.com\"@ or IP
  -- address).
  -> T.ServiceName
  -- ^ WebSockets server port (e.g., @\"443\"@ or @\"www\"@).
  -> B.ByteString
  -- ^ WebSockets resource (e.g., @\"/foo\/qux?bar=wat&baz\"@).
  --
  -- Leading @\'\/\'@ is optional.
  -> [(B.ByteString, B.ByteString)]
  -- ^ Extra HTTP Headers
  -- (e.g., @[(\"Authorization\", \"Basic dXNlcjpwYXNzd29yZA==\")]@).
  -> ((W.Connection, T.SockAddr) -> m r)
  -- ^ Computation to run after establishing a WebSockets to the remote
  -- server. Takes the WebSockets connection and remote end address.
  -> m r
connect hn sn res hds act = do
  T.connect hn sn $ \(sock, saddr) -> do
    Ex.bracket (streamFromSocket sock) (liftIO . W.close) $ \stream -> do
      conn <- clientConnectionFromStream stream hn sn res hds
      liftIO (W.forkPingThread conn 30)
      act (conn, saddr)

-- | Like 'connect', but connects to the destination server through a SOCKS5
-- proxy.
connectSOCKS5
  :: (MonadIO m, Ex.MonadMask m)
  => T.HostName -- ^ SOCKS5 proxy server hostname or IP address.
  -> T.ServiceName -- ^ SOCKS5 proxy server service port name or number.
  -> T.HostName
  -- ^ Destination WebSockets server hostname or IP address. We connect to this
  -- host /through/ the SOCKS5 proxy specified in the previous arguments.
  --
  -- Note that if hostname resolution on this 'T.HostName' is necessary, it
  -- will happen on the proxy side for security reasons, not locally.
  -> T.ServiceName
  -- ^ Destination WebSockets server port (e.g., @\"443\"@ or @\"www\"@).
  -> B.ByteString
  -- ^ WebSockets resource (e.g., @\"/foo\/qux?bar=wat&baz\"@).
  --
  -- Leading @\'\/\'@ is optional.
  -> [(B.ByteString, B.ByteString)]
  -- ^ Extra HTTP Headers
  -- (e.g., @[(\"Authorization\", \"Basic dXNlcjpwYXNzd29yZA==\")]@).
  -> ((W.Connection, T.SockAddr, T.SockAddr) -> m r)
  -- ^ Computation taking a 'W.Connection' for communicating with the
  -- destination WebSockets server through the SOCKS5 server, the address
  -- of that SOCKS5 server, and the address of the destination WebSockets
  -- server, in that order.
 -> m r
connectSOCKS5 phn psn dhn dsn res hds act = do
  T.connectSOCKS5 phn psn dhn dsn $ \(sock, pa, da) -> do
    Ex.bracket (streamFromSocket sock) (liftIO . W.close) $ \stream -> do
      conn <- clientConnectionFromStream stream dhn dsn res hds
      liftIO (W.forkPingThread conn 30)
      act (conn, pa, da)

-- | Obtain a 'W.Connection' to the specified 'Uri' over the given 'W.Stream',
-- connected to either a WebSockets server, or a Secure WebSockets server.
clientConnectionFromStream
  :: MonadIO m
  => W.Stream -- ^ Stream on which to establish the WebSockets connection.
  -> T.HostName
  -- ^ WebSockets server host name (e.g., @\"www.example.com\"@ or IP address).
  -> T.ServiceName -- ^ WebSockets server port (e.g., @\"443\"@ or @\"www\"@).
  -> B.ByteString
  -- ^ WebSockets resource (e.g., @\"/foo\/qux?bar=wat&baz\"@).
  --
  -- Leading @\'\/\'@ is optional.
  -> [(B.ByteString, B.ByteString)]
  -- ^ Extra HTTP Headers
  -- (e.g., @[(\"Authorization\", \"Basic dXNlcjpwYXNzd29yZA==\")]@).
  -> m W.Connection -- ^ Established WebSockets connection
clientConnectionFromStream stream hn sn res hds = liftIO $ do
  let res' :: String = '/' : dropWhile (=='/') (B8.unpack res)
      hds' :: W.Headers = map (first CI.mk) hds
      hnsn :: String = hn ++ ":" ++ sn
      wopts :: W.ConnectionOptions = W.defaultConnectionOptions
        { W.connectionStrictUnicode =
            False -- Slows stuff down. And see 'recv'.
        , W.connectionCompressionOptions =
            W.PermessageDeflateCompression
              W.defaultPermessageDeflate }
  W.newClientConnection stream hnsn res' wopts hds'

-- | Obtain a 'W.Stream' implemented using the network 'T.Socket'. You can
-- use the
-- [network-simple](https://hackage.haskell.org/package/network-simple)
-- library to get one of those.
streamFromSocket :: MonadIO m => T.Socket -> m W.Stream
streamFromSocket sock = liftIO $ do
  W.makeStream (T.recv sock 4096) (traverse_ (T.sendLazy sock))

-- | Receive bytes from the remote end.
--
-- Returns a strict 'BL.ByteString'.
--
-- Returns an empty string when the remote end gracefully closes the connection.

-- Note: The WebSockets protocol supports the silly idea of sending text, rather
-- than bytes, over the socket. We don't support that. If necessary, users can
-- find support for this in the `websockets` library.
recv :: MonadIO m => W.Connection -> m B.ByteString
{-# INLINABLE recv #-}
recv c = liftIO $ fix $ \k -> do
  ea <- Ex.try (W.receiveDataMessage c)
  case ea of
     Right (W.Binary bl) -> if BL.null bl then k else pure (BL.toStrict bl)
     Right (W.Text bl _) -> if BL.null bl then k else pure (BL.toStrict bl)
     Left (W.CloseRequest 1000 _) -> pure B.empty
     Left (W.CloseRequest 1001 _) -> pure B.empty
     Left e -> Ex.throw e

-- | Send bytes to the remote end.
--
-- Takes a lazy 'BL.ByteString'.

-- Note: The WebSockets protocol supports the silly idea of sending text, rather
-- than bytes, over the socket. We don't support that. If necessary, users can
-- find support for this in the `websockets` library.
send :: MonadIO m => W.Connection -> BL.ByteString -> m ()
{-# INLINABLE send #-}
send c = \bl -> case BL.null bl of
  True -> pure ()
  False -> liftIO (W.sendDataMessage c (W.Binary bl))

