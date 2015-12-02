{-# LANGUAGE OverloadedStrings #-}

{- This file is the Main program file for NomeBot, an IRC Chatbot
 - trained using Markov Chains to respond to IRC messages with
 - responses based on the corpus of yours truly.
 -
 - Author: Naomi Dickerson
 - E-mail: nld2@pdx.edu
 -}

module Main where

import System.IO
import Text.Printf
import Data.List
import System.Exit
import System.Environment
import Control.Monad
import Control.Monad.Reader
import Network
import Database.Persist.Sqlite
import Database.Persist.TH
import System.Random

import Bot
import Chat
import Trainer

-- The bot can be run in standard chat mode where it will log in to
-- an IRC server, join a specified channel, and listen for and reply
-- to messages indefinitely, or it can be run with the command line
-- flag "-t" to put it in training mode.
main = do
    runSqlite "markovchains.sqlite" $ do
        runMigration migrateAll 
    args <- getArgs
    dispatch args

dispatch :: [String] -> IO ()
dispatch ["-t"] = trainBot
dispatch _      = do
    conf <- configure
    bot  <- connect conf
    runReaderT run bot 

-- A bot can be configured with preset values. Eventually, these should
-- be moved to a configuration file or they should be able to be specified
-- on the command line
configure :: IO BotConfig 
configure = return $ BotConfig { server = "localhost", port = 6667
                               , nick = "NomeBot", chan = "#NomeBotChan" }

-- Creates a Bot with a socket connected to the given server and port,
-- and the configuration settings
connect :: BotConfig -> IO Bot
connect c = do
    sock <- connectTo (server c) $ PortNumber $ fromIntegral (port c)
    hSetBuffering sock NoBuffering
    return $ Bot sock c

-- Gets the config of the Bot and provides nick and user to the IRC
-- server and requests to join the specfied channel. Then listens on
-- the Bot's socket
run :: Irc ()
run = do
    cf <- asks config
    let n = nick cf
        c = chan cf
    write "NICK" n
    write "USER" (n++" 0 * :Nome Bot")
    write "JOIN" c
    asks socket >>= listen

-- Indefinitely check for incoming messages, print them to console,
-- and reply
listen :: Handle -> Irc ()
listen h = forever $ do
    h <- asks socket
    s <- liftIO $ hGetLine h
    liftIO $ putStrLn s
    reply s

