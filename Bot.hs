{- Bot 
 - Shared data types for NomeBot
 -}

module Bot where

import Control.Monad.Reader
import System.IO

data BotConfig = BotConfig 
    { training :: Bool
    , server :: String
    , port   :: Int
    , nick   :: String
    , chan   :: String
    , db     :: String }

data Bot  = Bot
    { socket :: Handle
    , config :: BotConfig
    }

nick' :: Irc String 
nick' = asks config >>= (return . nick) 

chan' :: Irc String
chan' = asks config >>= (return . chan)

db' :: Irc String
db' = asks config >>= (return . db)

-- A Monad transformer! Let's make a stack
-- of monads! Allow socket and config to be 
-- accessed without explicitly passing them
-- to every function along the way
-- i.e. create an environment
type Irc = ReaderT Bot IO
