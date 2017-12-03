{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE CPP #-}

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

    -- ** Folds and traversals
  , Foldable
    (
      foldMap
    , foldr
    , foldr'
    , foldl
    , foldl'
    , foldr1
    , foldl1
    )
    -- In base-4.8, these are instance methods.
  , elem
  , maximum
  , minimum
  , traverse_
  , sequenceA_
  , for_
  , maximumBy
  , minimumBy
  , Traversable
    (
      traverse
    , sequenceA
    , mapM
    , sequence
    )
  , for

    -- * Enhanced exports
    -- ** Simpler name for a typeclassed operation
  , map
  , empty
  , (++)
  , concat
  , intercalate
    -- ** Strict implementation
  , BasicPrelude.sum
  , BasicPrelude.product
    -- ** Text for Read and Show operations
  , tshow
  , fromShow
  , read
  , readIO
    -- ** FilePath for file operations
  , readFile
  , writeFile
  , appendFile

    -- * Text exports
    -- ** Text operations (Pure)
  , Text.lines
  , Text.words
  , Text.unlines
  , Text.unwords
  , textToString
  , ltextToString
  , fpToText
  , fpFromText
  , fpToString
  , encodeUtf8
  , decodeUtf8
    -- ** Text operations (IO)
  , getLine
  , getContents
  , interact

    -- * Miscellaneous prelude re-exports
    -- ** Math
  , Prelude.gcd
  , Prelude.lcm
    -- ** Show and Read
  , Prelude.Show (..)
  , Prelude.ShowS
  , Prelude.shows
  , Prelude.showChar
  , Prelude.showString
  , Prelude.showParen
  , Prelude.ReadS
  , Prelude.readsPrec
  , Prelude.readList
  , Prelude.reads
  , Prelude.readParen
  , Prelude.lex
  , readMay
    -- ** IO operations
  , getChar
  , putChar
  , readLn
  ) where

import CorePrelude

import Data.List hiding
  ( -- prefer monoid versions instead
    (++)
  , concat
  , intercalate
    -- prefer Text versions instead
  , lines
  , words
  , unlines
  , unwords
    -- prefer map = fmap instead
  , map
    -- prefer strict versions
  , sum
  , product
    -- prefer Foldable versions
  , elem
  , foldl
  , foldl'
  , foldl1
  , foldr
  , foldr1
  , maximum
  , minimum
  , maximumBy
  , minimumBy
  )

-- Import *all of the things* from Control.Monad,
-- specifically, the list-based things that
-- CorePrelude doesn't export
import Control.Monad hiding
  ( -- Also exported by Data.Traversable.
    mapM
  , sequence
  )


import Data.Foldable (Foldable(..), elem, maximum, minimum, traverse_, sequenceA_, for_)
import Data.Traversable (Traversable(..), for)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.IO as LText
import qualified Prelude
import Data.Text.Encoding (encodeUtf8, decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import qualified Safe

#if MIN_VERSION_base(4,10,0)
import Data.Foldable (maximumBy, minimumBy)
#else
maximumBy :: Foldable t => (a -> a -> Ordering) -> t a -> a
maximumBy cmp = foldl1 max'
  where max' x y = case cmp x y of
                     GT -> x
                     _  -> y

minimumBy :: Foldable t => (a -> a -> Ordering) -> t a -> a
minimumBy cmp = foldl1 min'
  where min' x y = case cmp x y of
                     GT -> y
                     _  -> x
#endif

-- | > map = fmap
map :: (Functor f) => (a -> b) -> f a -> f b
map = fmap

-- | > empty = mempty
empty :: Monoid w => w
empty = mempty
{-# DEPRECATED empty "Use mempty" #-}

infixr 5 ++

-- | > (++) = mappend
(++) :: Monoid w => w -> w -> w
(++) = mappend

-- | > concat = mconcat
concat :: Monoid w => [w] -> w
concat = mconcat

-- | > intercalate = mconcat .: intersperse
intercalate :: Monoid w => w -> [w] -> w
intercalate xs xss = mconcat (Data.List.intersperse xs xss)


-- | Compute the sum of a finite list of numbers.
sum :: (Foldable f, Num a) => f a -> a
sum = Data.Foldable.foldl' (+) 0

-- | Compute the product of a finite list of numbers.
product :: (Foldable f, Num a) => f a -> a
product = Data.Foldable.foldl' (*) 1


-- | Convert a value to readable Text
--
-- @since 0.6.0
tshow :: Show a => a -> Text
tshow = Text.pack . Prelude.show

-- | Convert a value to readable IsString
--
-- Since 0.3.12
fromShow :: (Show a, IsString b) => a -> b
fromShow = fromString . Prelude.show

-- | Parse Text to a value
read :: Read a => Text -> a
read = Prelude.read . Text.unpack

-- | The readIO function is similar to read
-- except that it signals parse failure to the IO monad
-- instead of terminating the program.
readIO :: (MonadIO m, Read a) => Text -> m a
readIO = liftIO . Prelude.readIO . Text.unpack


-- | Read a file and return the contents of the file as Text.
-- The entire file is read strictly.
readFile :: MonadIO m => FilePath -> m Text
readFile = liftIO . Text.readFile

-- | Write Text to a file.
-- The file is truncated to zero length before writing begins.
writeFile :: MonadIO m => FilePath -> Text -> m ()
writeFile p = liftIO . Text.writeFile p

-- | Write Text to the end of a file.
appendFile :: MonadIO m => FilePath -> Text -> m ()
appendFile p = liftIO . Text.appendFile p

textToString :: Text -> Prelude.String
textToString = Text.unpack

ltextToString :: LText -> Prelude.String
ltextToString = LText.unpack

-- | This function assumes file paths are encoded in UTF8. If it
-- cannot decode the 'FilePath', the result is just an approximation.
--
-- Since 0.3.13
fpToText :: FilePath -> Text
fpToText = Text.pack
{-# DEPRECATED fpToText "Use Data.Text.pack" #-}

-- |
-- Since 0.3.13
fpFromText :: Text -> FilePath
fpFromText = Text.unpack
{-# DEPRECATED fpFromText "Use Data.Text.unpack" #-}

-- |
-- Since 0.3.13
fpToString :: FilePath -> Prelude.String
fpToString = id
{-# DEPRECATED fpToString "Use id" #-}

-- | Note that this is /not/ the standard @Data.Text.Encoding.decodeUtf8@. That
-- function will throw impure exceptions on any decoding errors. This function
-- instead uses @decodeLenient@.
decodeUtf8 :: ByteString -> Text
decodeUtf8 = decodeUtf8With lenientDecode

getLine :: MonadIO m => m Text
getLine = liftIO Text.getLine

getContents :: MonadIO m => m LText
getContents = liftIO LText.getContents

interact :: MonadIO m => (LText -> LText) -> m ()
interact = liftIO . LText.interact

readMay :: Read a => Text -> Maybe a
readMay = Safe.readMay . Text.unpack

getChar :: MonadIO m => m Char
getChar = liftIO Prelude.getChar

putChar :: MonadIO m => Char -> m ()
putChar = liftIO . Prelude.putChar

-- | The 'readLn' function combines 'getLine' and 'readIO'.
readLn :: (MonadIO m, Read a) => m a
readLn = liftIO Prelude.readLn
