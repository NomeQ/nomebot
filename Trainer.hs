{- Trainer
 - Creates a database and table of markov chains from
 - a given text. Can be run by using NomeBot with -t command
 - line option. Recommended usage: cat sometxt.txt | ./NomeBot -t
 -}

{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}

module Trainer where

import Data.Char
import Data.List hiding (insert)
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Control.Monad
import Control.Monad.IO.Class

import Bot

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Markov
  chain String
  nextWord String
  deriving Show
|]

chainLength = 2
myDataBase = "markovchains.sqlite"

trainBot :: BotConfig -> IO ()
trainBot conf = do
    contents <- getContents
    let allWords = words $ map toLower $ goodChars contents
    runSqlite myDataBase $ do
        putChains allWords
        return ()

putChains [] = return ()
putChains ws@(x:xs)
    | length ws >= chainLength = putChain ws >> putChains xs 
    | otherwise                = return ()


putChain ws = insert $ Markov key value
    where
        key = (unwords . take chainLength) ws
        value = val' ws  

val' ws 
    | length ws > chainLength = ( head . drop chainLength) ws
    | otherwise               = "#"

goodChars :: [Char] -> [Char]
goodChars = repCtl . filter (isLatin1) . filter (/= '(') . filter (/= ')')

repCtl [] = []
repCtl (x:xs)
    | isControl x  = ' ' : (repCtl xs)
    | otherwise    = x : (repCtl xs) 

notBad :: Char -> Bool
notBad c = (isLatin1 c) && ((not . isControl) c) && ((/= '(') c) && ((/= ')') c)
