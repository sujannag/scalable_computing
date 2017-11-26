module Link.Types where

data User = User { username :: String }
            deriving (Show, Eq, Ord)

data Client = Client { 
	      clientUser :: User,
	      clientHandle :: Handle }
            deriving (Show, Eq)

data Server = Server {
	         serverUsers :: MVar (Map.Map User Cleint)
		}
