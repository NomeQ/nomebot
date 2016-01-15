{- NomeBot, An Irc Chatbot
 -
 - Author: Naomi Dickerson
 - Email : nld2@pdx.edu
 -
 - Final Project for CS 557
 -}

module Main where

import System.IO
import System.Environment
import System.Console.GetOpt
import System.Random
import Database.Persist.Sqlite
import Control.Monad
import Control.Monad.Reader
import Network

import Bot
import Trainer
import Chat

-- A default configuration which sets up a bot on a local server
-- To change, specify command line options
defaultConfig :: BotConfig
defaultConfig = BotConfig { training = False, server = "localhost"
                , port = 6667, nick = "NomeBot", chan = "#NomeBotChan"
                , db = "markovchains.sqlite" }

-- Print above usage instructions
header = "Usage: ./Nomebot [Options...]"

-- Command line options return list of functions on BotConfig
-- Note that specifying database does not currently work
options :: [OptDescr (BotConfig -> BotConfig)]
options = [ Option ['t'] ["train"] (NoArg 
              (\opt -> opt { training = True })) "Put bot in training mode"
          , Option ['s'] ["server"] (ReqArg 
              (\arg opt -> opt { server = arg }) "SERVER") "Specify server"
          , Option ['p'] ["port"] (ReqArg 
              (\arg opt -> opt { port = (read arg) }) "NUMBER") "Specify port"
          , Option ['n'] ["nick"] (ReqArg 
              (\arg opt -> opt { nick = arg }) "NICK") "Specify nick" 
          , Option ['c'] ["chan"] (ReqArg 
              (\arg opt -> opt { chan = arg }) "#CHANNEL") "Specify channel"
          , Option ['d'] ["database"] (ReqArg 
              (\arg opt -> opt { db = arg }) "FILE") "Specify database" ]


-- Create config from default config and command line options
configure fs = foldl overwrite defaultConfig fs  

-- e.g. update defaultConfig (\a opt -> opt { server = a } )
-- Reverses order so lambda in options can be used
overwrite c f = f c

-- Put bot in training mode, or run it on the IRC server depending on 
-- given configuration. Load the tables for markov chain key:value
dispatch :: BotConfig -> IO ()
dispatch conf = do
    runSqlite myDataBase $ do
        runMigration migrateAll
    if (training conf) then trainBot conf else do
       sock <- connectTo (server conf) $ PortNumber $ fromIntegral (port conf)
       hSetBuffering sock NoBuffering
       runReaderT run $ Bot sock conf
        
-- Connect to the Irc server and listen/reply forever until a
-- 'BotQuit' command is received
run :: Irc ()
run = do
    n <- nick'
    c <- chan'
    write "NICK" n
    write "USER" $ n++" 0 * :NomeBot"    
    write "JOIN" c
    h <- asks socket
    forever $ do
        s <- liftIO $ hGetLine h
        liftIO $ putStrLn s
        reply n s    

-- Get command line args and either error if incorrect, or run program
main = do
    args <- getArgs
    gen <- getStdGen
    case getOpt RequireOrder options args of
        (flags, [], [])  -> dispatch $ configure flags
        (_, badArgs, []) -> error $ "bad arguments: " ++ unwords badArgs
        (_, _, badFlags) -> error $ concat badFlags ++ usageInfo header options

    
