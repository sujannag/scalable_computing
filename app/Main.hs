module Main where

import Network.Socket
import System.IO
import System.Environment
import Control.Exception
import Control.Monad.Fix (fix)
import Control.Concurrent.ParallelIO.Local
import Control.Concurrent.MVar
import Control.Concurrent
import Data.List
import Data.List.Split
import qualified Data.HashTable.IO as H
import Control.Monad (when, unless)

import Client
import Utils

main :: IO ()
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
                 clientsNames = htClientsNames})

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
    sockHandler <- socketToHandle sock ReadWriteMode

    --
    hSetBuffering sockHandler LineBuffering

    --
    loopClient sockHandler originalSocket userPort chan clients chatRooms []

    --
    hClose sockHandler
    cLog "Client Disconnected "

{--
    

--}
loopClient :: Handle -> Socket -> String -> Chan Bool -> MVar Clients -> MVar ChatRooms -> [Int] -> IO ()
loopClient hdl originalSocket port killedChan clients chatrooms joinIds = do
    
    (input) <- getInput hdl killedChan joinIds clients
    
    let commandAndArgs = splitOn " " input
    let command = head commandAndArgs
    let args = intercalate " " $ tail commandAndArgs
    case command of
        "KILL_SERVICE"    -> do
            writeChan killedChan True
            threadDelay 200000 -- 200ms
            killService originalSocket

        "HELO"            -> do
            helo hdl args port
            loopClient hdl originalSocket port killedChan clients chatrooms joinIds

        "JOIN_CHATROOM:"  -> do
            (error, id) <- join hdl args port clients chatrooms
            
            if error then return ()
            else do
                if id `elem` joinIds then loopClient hdl originalSocket port killedChan clients chatrooms joinIds
                else loopClient hdl originalSocket port killedChan clients chatrooms (id:joinIds)

        "LEAVE_CHATROOM:" -> do
            (error, id) <- leave hdl args clients
            
            if error then return ()
            else do
                loopClient hdl originalSocket port killedChan clients chatrooms (delete id joinIds)

        "DISCONNECT:"     -> do
            error <- disconnect hdl args clients
            unless error (loopClient hdl originalSocket port killedChan clients chatrooms joinIds)

        "CHAT:"           -> do
            error <- chat hdl args clients
            unless error (loopClient hdl originalSocket port killedChan clients chatrooms joinIds)
        _                 -> otherCommand hdl input



