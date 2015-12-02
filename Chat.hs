{-# LANGUAGE OverloadedStrings #-}

module Chat where

import System.IO
import Text.Printf
import Data.List
import Data.Char
import System.Exit
import Control.Monad.Reader
import Control.Monad
import Database.Persist.Sqlite (runSqlite)
import Database.Persist
import System.Random

import Bot
import Trainer

-- The maximum length of a message
stopLength = 36
-- The mimimum length, after which NomeBot will end the message
-- at the next terminal punctuation mark it encounters
minLength = 5

-- Write to the socket handle in standard IRC syntax
write :: String -> String -> Irc ()
write s t = do
    h <- asks socket
    liftIO $ hPrintf h "%s %s\r\n" s t

-- Send a message to the channel
sendMsg :: String -> Irc ()
sendMsg t = do
    cf <- asks config
    let c = chan cf
    write "PRIVMSG" (c ++ " :" ++ t)

-- Upon receiving a message, reply. The bot's nick is
-- sent along with the message in order to ignore specific
-- server messages
reply :: String -> Irc ()
reply t = do
    c <- asks config 
    let n = nick c
    getReply t n

-- Reply to server pings, ignore messages from yourself or from
-- the channel
getReply :: String -> String -> Irc ()
getReply t n  
    | "PING" `isPrefixOf` t       = write "PONG" (':' : drop 6 t)
    | ":irc" `isPrefixOf` t       = return ()
    | (':' : n) `isPrefixOf` t    = return ()
    | otherwise                   = respond t

-- Allows anyone to ask NomeBot to log out and terminate
respond :: String -> Irc ()
respond x 
    | "BotQuit" `isPrefixOf` (clean x) = botQuit
    | otherwise                        = getResponse $ clean x

botQuit :: Irc ()
botQuit = do 
    write "QUIT" ":Received BotQuit"
    liftIO $ exitWith ExitSuccess

-- Scan for a valid key in the received message. If none,
-- grab a random key from the table. Chain together a response
-- from the starting key and then send it.
getResponse :: String -> Irc ()
getResponse s = do
    k <- scanKey $ ( reverse . words . map toLower ) s
    startK <- randFrom k
    resp <- chainReply $ words startK
    sendMsg $ makePretty resp

-- TODO make compatible with variable length chains
-- Starting from the back of the received message, look for matching
-- keys in the table. If no key is found, get a random key 
scanKey ws@(x:y:z:zs) = do
    possKey <- getMarkovs $ unwords $ reverse $ take chainLength ws
    case possKey of
        [] -> scanKey (y:z:zs)
        _  -> return $ map getKeys possKey
scanKey _  = do
    es <- randChain 
    return $ [markovChain es]   

-- Get a list of all entries in the Markov table with the same 'Chain'
-- If none, will return an empty list
getMarkovs ws = liftIO $ runSqlite "markovchains.sqlite" $ do
   nxt <- selectList [MarkovChain ==. ws] [] 
   return nxt 

-- Extract a key (Chain) from an Entity Markov
getKeys = markovChain . entityVal
-- Extract a value (NextWord) from an Entity Markov
getVals = markovNextWord . entityVal

-- Get the number of rows in the table, and get a random
-- number in that range to use as key (ID) for selecting
-- a random row (Entity Markov)
randChain = liftIO $ runSqlite "markovchains.sqlite" $ do
    c <- count [MarkovChain !=. " "]
    idx <- liftIO $ getRandIndex $ c - 1
    a <- get $ wrapKey idx
    let b = fromJust a
    return b

-- A little dangerous to be ignoring Nothing, but the value
-- used should be a valid ID and the program should probably
-- crash if this isn't functioning properly
fromJust :: Maybe m -> m
fromJust (Just a) = a

-- Turn an Int into a valid key for querying the database
wrapKey :: Int -> Key Markov
wrapKey a = MarkovKey $ fromIntegral a

-- Select a random element of a list 
randFrom ws = liftIO $ do
    let upperB = (length ws) - 1
    idx <- getRandIndex upperB
    return $ ws !! idx

-- Generate a random number between 0 and an upper bound
getRandIndex :: Int -> IO Int
getRandIndex bound = do
    gen <- getStdGen 
    let (randNumber, newGen) = randomR (0,bound) gen :: (Int, StdGen)
    gen' <- newStdGen
    return randNumber

-- Chain new words to the reply until it exceeds the stop length
chainReply ws
    | length ws >= stopLength = return ws
    | otherwise               = do addw <- chainWord ws; chainReply' ws addw

-- If the new word is the stop character or ends in terminal punctuation
-- and the reply is greater than the min length, stop, otherwise continue
-- adding new words to the reply
chainReply' ws w
    | (w == "#") && (length ws >= minLength)   = return ws
    | (isEnding w) && (length ws >= minLength) = return $ ws ++ [w]
    | w == "#"                                 = do addNew <- chainWord' ws []; chainReply $ ws ++ [addNew]
    | otherwise                                = chainReply $ ws ++ [w]
    
-- Grab the last (chainLength) worth of words from the reply and use them 
-- as a key for finding the next word
chainWord ws = do
    let key = takeLast chainLength ws
    nexts <- getMarkovs $ unwords key
    chainWord' ws nexts

-- If the words are not a key, get a new random word from the table
-- Otherwise, randomly select one of the possible nextWords from the
-- given key
chainWord' ws [] = do 
    new <- randChain
    return $ markovNextWord new
chainWord' ws nexts = do
    next <- randFrom $ map getVals nexts   
    return next
            
takeLast n = reverse . take n . reverse

-- A word is an ending word if it ends in 
-- a terminal punctuation mark.
isEnding :: [Char] -> Bool
isEnding [] = False
isEnding (x:[]) = x `elem` puncs
isEnding (x:xs) = isEnding xs

-- Functions for cleaning up reply messages 

puncs = ['.', '?', '!']

clean = drop 1 . dropWhile (/= ':') . drop 1

makePretty = capitalizeFirst . unwords

capitalizeFirst [] = []
capitalizeFirst (x:xs) = capitalizeFirst' $ (toUpper x) : xs

capitalizeFirst' [] = []
capitalizeFirst' (x:y:z:zs)
    | (x `elem` puncs) && (y == ' ') = x : y : (toUpper z) : (capitalizeFirst' zs)
    | otherwise                      = x : (capitalizeFirst' (y:z:zs))
capitalizeFirst' xs                  = xs


