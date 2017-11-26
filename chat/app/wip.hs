module Main where

import Lib

module Link.Types where


main :: IO ()
main = someFunc

connectClient :: Server -> Handle -> IO ()
connectClient server handle = do
    hSetNewLineMode handle universalNewLineMode
	hSetBuffering handle LineBuffering
	readName
	where

readName :: IO ()
readName = do
    name <- hGetLine handle
	if null name
	then readName
	else do
	    let user = User name
		ok <- checkAddClient server user handle
		case ok of
		    Nothing -> do
			    hPrintf handle
				    "The name %s is in use, please use another\n" name
				readName
			Just client ->
				runClient server client
					`finally` removeClient server user

checkAddClient :: Server -> User -> Handle -> IO (Maybe Client)
checkAddClient Server {..} user@User {..} handle = 
    modifyMVar serverUsers $ \clientMap ->
	if Map.member
