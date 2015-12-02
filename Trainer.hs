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

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Markov
  chain String
  nextWord String
  deriving Show
|]

chainLength = 2

trainBot :: IO ()
trainBot = do
    contents <- getContents
    let allWords = words $ map toLower $ asciiOnly contents
    runSqlite "markovchains.sqlite" $ do
        runMigration migrateAll
        putChains allWords
        return ()

-- Using chains of length 2, only add if at least 3 elements of list
putChains ws@(w:x:ys) = do
    putChain ws
    putChains (x:ys)
putChains _ = return ()

-- We're assured when this is called that the list has at least 3 
-- elements, but we don't know if it has a fourth. Make sure list
-- indexing does not fail.
putChain ws = insert $ Markov key value
    where
        key = (unwords . take chainLength) ws
        value = val' ws  

-- If there are four or more elements, insert the next word for val,
-- otherwise enter the stop symbol.
val' (x:y:z:_) = z
val' _ = "#"

-- Remove characters incompatible with IRC
asciiOnly :: String -> String
asciiOnly [] = []
asciiOnly (x:xs)
    | isAscii x = x : (asciiOnly xs)
    | otherwise = asciiOnly xs
