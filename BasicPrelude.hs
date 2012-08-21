{-# LANGUAGE NoImplicitPrelude #-}

module BasicPrelude
  ( module CorePrelude
  , module Data.List
  , module Prelude
  , module Data.Text
  , module Data.Text.Lazy.IO
  , module Control.Monad
  , map
  , show
  , read
  , sum
  , product
  , putStr
  , getLine
  , getContents
  , readFile
  , writeFile
  , appendFile
  , readIO
  , empty
  , (++)
  , concat
  ) where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.IO as TL
import qualified Prelude as P
import qualified Filesystem.Path.CurrentOS as F

import CorePrelude

import Data.List hiding
  ( -- prefer monoid versions instead
    (++)
  , concat
  
    -- prefer Text versions instead
  , lines
  , words
  , unlines
  , unwords
  , intercalate
  
    -- prefer map = fmap instead
  , map
    
    -- prefer strict versions
  , sum
  , product
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
  , putChar
  , getChar
  , readLn
  )

import Data.Text
  ( lines
  , words
  , unlines
  , unwords
  , intercalate
  )

import Data.Text.Lazy.IO
  ( interact
  )

-- Import *all of the things* from Control.Monad,
-- specifically, the list-based things that
-- CorePrelude doesn't export
import Control.Monad


map :: (Functor f) => (a -> b) -> f a -> f b
map = fmap

show :: Show a => a -> Text
show = T.pack . P.show

read :: Read a => Text -> a
read = P.read . T.unpack

sum :: Num a => [a] -> a
sum = foldl' (+) 0

product :: Num a => [a] -> a
product = foldl' (*) 1

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


empty :: Monoid w => w
empty = mempty

infixr 5 ++
(++) :: Monoid w => w -> w -> w
(++) = mappend

concat :: Monoid w => [w] -> w
concat = mconcat
