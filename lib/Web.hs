module Web (runServer) where

  import Control.Concurrent
  import Network.Socket
  import System.IO

  runServer :: (String -> IO String) -> IO ()
  runServer handleMsg = do
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet 4242 iNADDR_ANY)
    listen sock 2
    serverLoop sock handleMsg

  serverLoop :: Socket -> (String -> IO String) -> IO ()
  serverLoop sock handleMsg = do
    conn <- accept sock
    forkIO $ runConn conn handleMsg
    serverLoop sock handleMsg

  runConn :: (Socket, SockAddr) -> (String -> IO String) -> IO ()
  runConn (sock, _) handleMsg = do
    handle <- socketToHandle sock ReadWriteMode
    hSetBuffering handle NoBuffering
    msg <- hGetContents handle
    response <- handleMsg msg
    hPutStrLn handle response