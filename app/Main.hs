module Main where

import Network.Socket
import System.IO
import System.Environment          -- to include getArgs
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.ParallelIO.Local
import Data.List
import qualified Data.HashTable.IO as H

import Utils

main :: IO()
main = do

    -- take the port number from the user
    [userPort] <- getArgs

    -- create a scoket
    sock <- socket AF_INET Stream 0

    -- make socket reusable
    setSocketOption sock ReuseAddr 1

    -- Listen on the TCP port given by the user
    bind sock (SockAddrInet (read userPort) iNADDR_ANY)

    let nThreads = 5
    listen sock (nThreads*2)

    -- make a new channel
    chan <- newChan

    -- Use a hashtable to maintain copy of the clients
    htSI             <- H.new :: IO (HashTable String Int)
    htIC             <- H.new :: IO (HashTable Int ChatRoom)
    htClients        <- H.new :: IO (HashTable Int Client)
    htClientsNames   <- H.new :: IO (HashTable String Int)

    let nCR = 0

    chatRooms <- newMVar (ChatRooms {chatRoomFromId = htIC,
                                     chatRoomIdFromName = htSI,
				     numberOfChatRooms = nCR})

    clients <- newMVar (Clients {lastClientId = 0, 
                                 theClients = htClients,
				 clientNames = htClientsNames})

    -- create the threads
    withPool nThreads $ \pool -> parallel_ pool(replicate nThreads (server sock userPort chan clients chatRooms))

    -- end of program
    cLog "End of server"


{--

--}
server :: Socket -> String -> Chan Bool -> MVar Clients -> MVar ChatRooms -> IO()
server sock userPort chan clients chatRooms = do

    cLog "In server thread, waiting for a connection"
    
    -- accpet the socket. What if the connection is not accepted
    conn <- accept sock
    cLog "Connection accepted!!"
    
    -- run the client logic
    runClient conn sock userPort chan clients chatRooms

    -- repeat
    server sock userPort chan clients chatRooms

{--

--}
runClient :: (Socket, SockAddr) -> Socket -> String -> Chan Bool -> MVar Clients -> MVar ChatRooms -> IO()
runClient (sock, addr) originalSocket userPort chan clients chatRooms = do
    
    cLog "In runClient!!"
    
    --
    handler <- socketToHandle sock ReadWriteMode

    --
    hSetBuffering handler LineBuffering

    --
    loopClient handler originalSocket userPort chan clients chatRooms []

    --
    hClose handler
    cLog "Client Disconnected "


{--

--}
loopClient :: Handle -> Socket -> String -> Chan Bool -> MVar Clients -> MVar ChatRooms -> [Int] -> IO()
loopClient handler originalSocket userPort chan clients chatRooms joinIds = do

    cLog "In loopClient"
    -- wait for an input from the client
    

    -- what if the client times out?

    -- if all is well, handle the inputs

    



