{-# LANGUAGE NoImplicitPrelude #-}

-- | BasicPrelude mostly re-exports
-- several key libraries in their entirety.
-- The exception is Data.List,
-- where various functions are replaced
-- by similar versions that are either
-- generalized, operate on Text,
-- or are implemented strictly.
module BasicPrelude
  ( -- * Module exports
    module CorePrelude
  , module Data.List
  , module Control.Monad

    -- * Novel exports
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

    -- * Text exports
  , Text.lines
  , Text.words
  , Text.unlines
  , Text.unwords
  , Text.intercalate
  , LText.interact

    -- * Prelude re-exports
  , Prelude.Bounded (..)
  , Prelude.gcd
  , Prelude.lcm
  , Prelude.String
  , Prelude.ReadS
  , Prelude.ShowS
  , Prelude.Read (..)
  , Prelude.Show (showsPrec, showList)
  , Prelude.reads
  , Prelude.shows
  , Prelude.showChar
  , Prelude.showString
  , Prelude.showParen
  , Prelude.readParen
  , Prelude.lex
  , Prelude.IOError
  , Prelude.ioError
  , Prelude.userError
  , Prelude.putChar
  , Prelude.getChar
  , Prelude.readLn
  ) where

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

-- Import *all of the things* from Control.Monad,
-- specifically, the list-based things that
-- CorePrelude doesn't export
import Control.Monad

import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Lazy.IO as LText 
import qualified Filesystem.Path.CurrentOS as FilePath
import qualified Prelude



map :: (Functor f) => (a -> b) -> f a -> f b
map = fmap

show :: Show a => a -> Text
show = Text.pack . Prelude.show

read :: Read a => Text -> a
read = Prelude.read . Text.unpack

sum :: Num a => [a] -> a
sum = foldl' (+) 0

product :: Num a => [a] -> a
product = foldl' (*) 1

putStr :: Text -> IO ()
putStr = Text.putStr

getLine :: IO Text
getLine = Text.getLine

getContents :: IO LText
getContents = LText.getContents

readFile :: FilePath -> IO Text
readFile = Text.readFile . FilePath.encodeString

writeFile :: FilePath -> Text -> IO ()
writeFile = Text.writeFile . FilePath.encodeString

appendFile :: FilePath -> Text -> IO ()
appendFile = Text.appendFile . FilePath.encodeString

readIO :: Read a => Text -> IO a
readIO = Prelude.readIO . Text.unpack


empty :: Monoid w => w
empty = mempty

infixr 5 ++
(++) :: Monoid w => w -> w -> w
(++) = mappend

concat :: Monoid w => [w] -> w
concat = mconcat

