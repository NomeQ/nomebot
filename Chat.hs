{- Chat
 - Functions for constructing and sending replies
 - Queries established sqlite database
 - and constructs random, markov chain messages -}

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

-- The minimum and maximum length of a message
maxLength = 46
minLength = 5

-- Write to the socket handle in standard IRC syntax
write :: String -> String -> Irc ()
write s t = do
    h <- asks socket
    liftIO $ hPrintf h "%s %s\r\n" s t

-- Reply only to messages sent on your channel from
-- another user
reply :: String -> String -> Irc ()
reply n s 
    | "PING" `isPrefixOf` s            = pong s
    | ":irc" `isPrefixOf` s            = return ()
    | (':' : n) `isPrefixOf` s         = return ()
    | "BotQuit" `isPrefixOf` (clean s) = botQuit
    | otherwise                        = sendRsp $ clean s

-- Respond to server pings
pong :: String -> Irc ()
pong s = write "PONG" $ ':' : drop 6 s

botQuit :: Irc ()
botQuit = do 
    write "QUIT" ":Received BotQuit"
    liftIO $ exitWith ExitSuccess

-- Scan for a valid key in the received message. If none,
-- grab a random key from the table. Chain together a response
-- from the starting key and then send it.
sendRsp :: String -> Irc ()
sendRsp s = do
    k <- scanKey $ ( reverse . words . map toLower ) s
    startK <- randFrom k
    resp <- chainReply $ words startK
    let msg = makePretty resp
    c <- chan'
    write "PRIVMSG" (c ++ " :" ++ msg)


scanKey ws
    | length ws < chainLength = do es <- randRow; return $ [markovChain es]
    | otherwise               = possKey ws

possKey ws@(x:xs) = do
    pk <- getRows $ (unwords . reverse . take chainLength) ws
    case pk of
        [] -> scanKey xs
        _  -> return $ map getKeys pk

-- Get a list of all entries in the Markov table with the same 'Chain'
-- If none, will return an empty list
getRows ws = liftIO $ runSqlite myDataBase $ do
   nxt <- selectList [MarkovChain ==. ws] [] 
   return nxt 

-- Get the number of rows in the table, and get a random
-- number in that range to use as key (ID) for selecting
-- a random row (Entity Markov)
randRow = liftIO $ runSqlite myDataBase $ do
    c <- count [MarkovChain !=. " "]
    idx <- liftIO $ getRandIndex $ c - 1
    a <- get $ wrapKey idx
    let b = fromJust a
    return b

-- Select a random element of a list 
randFrom ws = liftIO $ do
    let upperB = (length ws) - 1
    idx <- getRandIndex upperB
    return $ ws !! idx

-- Generate a random number between 0 and an upper bound
getRandIndex :: Int -> IO Int
getRandIndex bound = do
    gen <- newStdGen 
    let (randNumber, newGen) = randomR (0,bound) gen :: (Int, StdGen)
    return randNumber

-- Extract a key (Chain) from an Entity Markov
getKeys = markovChain . entityVal
-- Extract a value (NextWord) from an Entity Markov
getVals = markovNextWord . entityVal

-- A little dangerous to be ignoring Nothing, but the value
-- used should be a valid ID and the program should probably
-- crash if this isn't functioning properly
fromJust :: Maybe m -> m
fromJust (Just a) = a

-- Turn an Int into a valid key for querying the database
wrapKey :: Int -> Key Markov
wrapKey a = MarkovKey $ fromIntegral a

-- Chain new words to the reply until it exceeds the stop length
chainReply ws
    | length ws >= maxLength = return ws
    | otherwise              = chainWord ws >>= chainReply' ws

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
    nexts <- getRows $ unwords key
    chainWord' ws nexts

-- If the words are not a key, get a new random word from the table
-- Otherwise, randomly select one of the possible nextWords from the
-- given key
chainWord' ws [] = do 
    new <- randRow
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
    | (x == ' ') && (y == 'i') && (z == ' ') = x : 'I' : z : (capitalizeFirst' zs)
    | otherwise                      = x : (capitalizeFirst' (y:z:zs))
capitalizeFirst' xs                  = xs


