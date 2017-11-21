module Main where

import Network.Socket

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

mainLoop :: Socket -> IO()
mainLoop sock = do		-- takes sock as an argument
	-- accept a socket connection
	conn <- accept sock

	-- Server logic. It also closes the socket
	runConn conn

	-- repeat
	mainLoop sock


runConn :: (Socket, SockAddr) -> IO ()
runConn (sock, _) = do

	-- sends a message to the client
	send sock "Hello!\n"

	-- closes the socket connection
	close sock