module Bot where

import Control.Monad.Reader
import System.IO

data BotConfig = BotConfig 
    { server :: String
    , port   :: Int
    , nick   :: String
    , chan   :: String
    }

data Bot  = Bot
    { socket :: Handle
    , config :: BotConfig
    }

-- A Monad transformer! Let's make a stack
-- of monads! Allow socket and config to be 
-- accessed without explicitly passing them
-- to every function along the way
type Irc = ReaderT Bot IO
