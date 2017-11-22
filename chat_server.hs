module Main where

import Network.Socket
import System.IO
import Control.Concurrent
import Control.Exception
import Control.Monad (when)
import Control.Monad.Fix (fix)

 -- Code added to facilitate communication between threads

main :: IO()
main = do
	-- let value = 10
	--putStrLn ("Hello!!" ++ value)

	-- create a socket
	sock <- socket AF_INET Stream 0

	-- make socket reusable
	setSocketOption sock ReuseAddr 1

	--listen on TCP port 4242
	bind sock (SockAddrInet 4242 iNADDR_ANY)

	-- set a max of two queued connections
	listen sock 2

	-- Doesnt return, keep on listening iteratively.
	-- newChan :: IO (Chan a)
	chan <- newChan		--newChan builds and returns a new instance of Chan
	
	-- why?
	_ <- forkIO $ fix $ \loop -> do
		(_, _) <- readChan chan
		loop
	mainLoop sock chan 0

type Msg = (Int, String)

-- mainLoop function
mainLoop :: Socket -> Chan Msg -> Int -> IO()
mainLoop sock chan msgNum = do		-- takes sock as an argument
	-- accept a socket connection
	conn <- accept sock
	putStrLn ("Connection from client accepted \n")

	-- Server logic. It also closes the socket
	forkIO (runConn conn chan msgNum)	-- each connection starts in its own thread

	-- repeat
	mainLoop sock chan $! msgNum + 1

-- runConn function
runConn :: (Socket, SockAddr) -> Chan Msg  -> Int ->IO ()

-- use system IOs instead of send and recv
runConn (sock, _) chan msgNum = do

	-- writeChan :: Chan a -> a -> IO ()
	-- Write a value to Chan, in our case msg to chan
	let broadcast msg = writeChan chan (msgNum, msg)
	hdl <- socketToHandle sock ReadWriteMode	--no idea
	hSetBuffering hdl NoBuffering			--no idea
	
	-- Send print messages
	hPutStrLn hdl "Hi, what's your name?"
	name <- fmap init (hGetLine hdl)
	broadcast ("---> " ++ name ++ " entered chat.")
	hPutStrLn hdl ("Welcome, " ++ name ++ "!")

	-- Duplicate a channel, Data written by anyone is seen by everyone else.
	-- dupChan :: Chan a -> IO (Chan a)
	commLine <- dupChan chan

	--fork off a thread for reading from the duplicate channel
	reader <- forkIO $ fix $ \loop -> do
		--line <- fmap init(hGetLine hdl)		-- no idea
		(nextNum, line) <- readChan commLine
		when (msgNum /= nextNum) $ hPutStrLn hdl line
		--hPutStrLn hdl line
		loop
	
	-- handle exceptions
	handle(\(SomeException _) -> return()) $ fix $ \loop -> do
		line <- fmap init (hGetLine hdl)
		case line of 
			-- if an exception is caought, send a message
			"quit" -> hPutStrLn hdl "Bye!"
			--else loop
			_ -> broadcast (name ++ ": " ++ line) >> loop

	killThread reader                      -- kill after the loop ends
    	broadcast ("<-- " ++ name ++ " left.") -- make a final broadcast
    	hClose hdl                             -- close the handle

