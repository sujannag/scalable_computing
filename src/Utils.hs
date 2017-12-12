module Utils where

import Network.BSD
import System.IO
import System.Directory
import Control.Exception
import Control.Monad.Fix (fix)
import Control.Concurrent
import Data.IP
import Data.List
import qualified Data.Maybe as M
import Data.String.Utils
import Foreign.Ptr
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc (mallocBytes, free)
import qualified Data.HashTable.IO as H

type HashTable k v = H.CuckooHashTable k v

data Client    = Client { clientName :: String
                        , subs       :: HashTable Int (Chan String)
                        , joinId     :: Int
                        }
data Clients   = Clients { lastClientId   :: Int
                         , theClients     :: HashTable Int Client
                         , clientsNames   :: HashTable String Int
                         }
data ChatRoom  = ChatRoom Int (Chan String)
data ChatRooms = ChatRooms { chatRoomFromId     :: HashTable Int ChatRoom
                           , chatRoomIdFromName :: HashTable String Int
                           , numberOfChatRooms  :: Int  
                           }

getWaitBySocket :: Int
getWaitBySocket = 80000 -- 80ms

nonBlockingRead :: Handle -> String -> IO String
nonBlockingRead hdl currentS = do
  buf <- mallocBytes 4096 :: IO (Ptr CChar)
  nbRead <- hGetBufNonBlocking hdl buf 4096 
  request <- peekCStringLen (buf, nbRead)
  free buf
  if nbRead == 0 then do
    threadDelay getWaitBySocket
    return currentS
  else do
    next <- nonBlockingRead hdl (currentS ++ request)
    return next

readChansClient :: [(String, Int)] -> (Int, Chan String) -> IO [(String, Int)]
readChansClient current (ref, chan) = do
  canPass <- isEmptyChan chan
  if canPass then return current
  else do
    toSend <- readChan chan
    return ((toSend, ref):current)

unpackAndReadChansClient :: Handle -> MVar Clients -> Int -> IO ()
unpackAndReadChansClient hdl clients joinId = do

  (Clients lastClientId theClients clientsNames) <- takeMVar clients
  maybeClient <- H.lookup theClients joinId
  
  let (Client _ channels _) = M.fromJust maybeClient
  messages <- H.foldM readChansClient [] channels
  
  let messagesStr = map (\(m,ref) -> m) $ sortOn (\(m,ref) -> ref) messages
  mapM_ (sendResponse hdl) messagesStr
  putMVar clients (Clients lastClientId theClients clientsNames)

{--
Read data from the channels
--}
readChans :: Handle -> [Int] -> MVar Clients -> IO ()
readChans hdl joinIds clients = do
  mapM_ (unpackAndReadChansClient hdl clients) joinIds

{--
 Gets the inputs from the clients 
--}
getInput :: Handle -> Chan Bool  -> [Int] -> MVar Clients -> IO (String)
getInput hdl killedChan joinIds clients = do
  readChans hdl joinIds clients
  request <- handle (\(SomeException _) -> return "") $ fix $ (return $ nonBlockingRead hdl "")
  if null request then do
      res <- getInput hdl killedChan joinIds clients
      return res
  else do
      return (clean request)

sendResponse :: Handle -> String -> IO ()
sendResponse hdl resp = do
    hSetBuffering hdl $ BlockBuffering $ Just (length resp)
    hPutStr hdl resp

{--
Used to send error codes
--}
sendError :: Handle -> Int -> String -> IO ()
sendError hdl errorCode errorString = sendResponse hdl $ "ERROR_CODE: " ++ (show errorCode) ++ "\nERROR_DESCRIPTION: " ++ errorString

{--
Returns the IP address of the Host
--}
getSysHostName :: IO String
getSysHostName = do
    host <- getHostName >>= getHostByName
    return $ show $ fromHostAddress $ head $ hostAddresses host

{--
		
--}
clean :: String -> String
clean input = init input

{--
		
--}
sendLeaveMessages :: String -> [(String, Chan String, Int)] -> (Int, Chan String) -> IO [(String, Chan String, Int)]
sendLeaveMessages clientName currentRes (chatRoomRef, chan) = do
  let message = "CHAT: " ++ (show chatRoomRef) ++ "\nCLIENT_NAME: " ++ clientName ++ "\nMESSAGE: " ++ clientName ++ " has left this chatroom.\n\n"
  -- writeChan chan message
  return (currentRes ++ [(message, chan, chatRoomRef)])

cLog :: String -> IO ()
cLog s = putStrLn $ "\n*****************\n" ++ s ++ "\n*****************"
