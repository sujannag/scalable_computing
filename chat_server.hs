module Main where

import Network.Socket
import System.IO

main :: IO()
main = do
	-- create a socket
	sock <- socket AF_INET Stream 0

	-- make socket reusable
	setSocketOption sock ReuseAddr 1

	--listen on TCP port 4242
	bind sock (SockAddrInet 4242 iNADDR_ANY)

	-- set a max of two queued connections
	listen sock 2

	-- Doesnt return, keep on listening iteratively.
	mainLoop sock

-- mainLoop function
mainLoop :: Socket -> IO()
mainLoop sock = do		-- takes sock as an argument
	-- accept a socket connection
	conn <- accept sock

	-- Server logic. It also closes the socket
	runConn conn

	-- repeat
	mainLoop sock

-- runConn function
runConn :: (Socket, SockAddr) -> IO ()

-- use system IOs instead of send and recv
runConn (sock, _) = do
	hdl <- socketToHandle sock ReadWriteMode
	hSetBuffering hdl NoBuffering
	hPutStrLn hdl "Hello!"
	hClose hdl

