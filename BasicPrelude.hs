{-# LANGUAGE NoImplicitPrelude #-}

module BasicPrelude
  ( module CorePrelude
  , module Data.List
  , module Prelude
  , module Data.Text
  , module Control.Monad
  , show
  , read
  , putStr
  , getLine
  , getContents
  , readFile
  , writeFile
  , appendFile
  , readIO
  ) where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.IO as TL
import qualified Prelude as P
import qualified Filesystem.Path.CurrentOS as F

import CorePrelude

import Data.List hiding
  ( -- already in CorePrelude
    (++)
  , concat
  
    -- prefer Text versions instead
  , lines
  , words
  , unlines
  , unwords
  , intercalate
  )

import Prelude
  ( Bounded (..)
  , gcd
  , lcm
  , seq
  , ($!)
  , curry
  , until
  , asTypeOf
  , undefined
  , String
  , ReadS
  , ShowS
  , Read (..)
  , Show (showsPrec, showList)
  , reads
  , shows
  , showChar
  , showString
  , showParen
  , readParen
  , lex
  , IOError
  , ioError
  , userError
  , catch -- deprecated
  , putChar
  , getChar
  , readLn
  , interact
  )

import Data.Text
  ( lines
  , words
  , unlines
  , unwords
  , intercalate
  )

-- Import *all of the things* from Control.Monad,
-- specifically, the list-based things that
-- CorePrelude doesn't export
import Control.Monad


show :: Show a => a -> Text
show = T.pack . P.show

read :: Read a => Text -> a
read = P.read . T.unpack

putStr :: Text -> IO ()
putStr = T.putStr

getLine :: IO Text
getLine = T.getLine

getContents :: IO LText
getContents = TL.getContents

readFile :: FilePath -> IO Text
readFile = T.readFile . F.encodeString

writeFile :: FilePath -> Text -> IO ()
writeFile = T.writeFile . F.encodeString

appendFile :: FilePath -> Text -> IO ()
appendFile = T.appendFile . F.encodeString

readIO :: Read a => Text -> IO a
readIO = P.readIO . T.unpack

