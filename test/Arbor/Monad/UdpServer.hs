module Arbor.Monad.UdpServer
  ( runUdpServer
  , UdpHandler
  ) where

import Network.Socket

import qualified Data.ByteString           as BS
import qualified Network.Socket.ByteString as BS

type UdpHandler = SockAddr -> BS.ByteString -> IO ()

runUdpServer :: ()
  => String       -- ^ Port number or name; 514 is default
  -> UdpHandler  -- ^ Function to handle incoming messages
  -> IO ()
runUdpServer port handler = withSocketsDo $ do
  addrinfos <- getAddrInfo
              (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
              Nothing (Just port)
  let serveraddr = head addrinfos

  sock <- socket (addrFamily serveraddr) Datagram defaultProtocol

  bind sock (addrAddress serveraddr)

  procMessages sock
  where procMessages sock = do
          (msg, addr) <- BS.recvFrom sock 1024
          handler addr msg
          procMessages sock
