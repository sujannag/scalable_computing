module Utils where

import Network.BSD
import System.IO
import Control.Concurrent
import qualified Data.HashTable.IO as H

type HashTable k v = H.CuckooHashTable k v

data Client = Client { clientName :: String,
                       subs :: HashTable Int (Chan String),
		       joinId :: Int}

data Clients = Clients { lastClientId :: Int,
                       theClients :: HashTable Int Client,
		       clientNames :: HashTable String Int}

data ChatRoom = ChatRoom Int (Chan String)
data ChatRooms = ChatRooms {chatRoomFromId :: HashTable Int ChatRoom,
                            chatRoomIdFromName :: HashTable String Int,
			    numberOfChatRooms :: Int}


cLog :: String -> IO ()
cLog s = putStrLn $ "\n*******\n" ++ s ++ "\n********"
