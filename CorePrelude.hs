{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE CPP #-}

module CorePrelude
    ( -- * Standard
      -- ** Operators
      (Prelude.$)
    , (Prelude.$!)
    , (Prelude.&&)
    , (Prelude.||)
    , (Control.Category..)
      -- ** Functions
    , Prelude.not
    , Prelude.otherwise
    , Prelude.fst
    , Prelude.snd
    , Control.Category.id
    , Prelude.maybe
    , Prelude.either
    , Prelude.flip
    , Prelude.const
    , Prelude.error
    , putStrLn
    , getArgs
    , Prelude.odd
    , Prelude.even
    , Prelude.uncurry
    , Prelude.curry
    , Data.Tuple.swap
    , Prelude.until
    , Prelude.asTypeOf
    , Prelude.undefined
    , Prelude.seq
      -- ** Type classes
    , Prelude.Ord (..)
    , Prelude.Eq (..)
    , Prelude.Bounded (..)
    , Prelude.Enum (..)
    , Prelude.Show
    , Prelude.Read
    , Prelude.Functor (..)
    , Prelude.Monad (..)
    , (Control.Monad.=<<)
    , Data.String.IsString (..)
      -- ** Numeric type classes
    , Prelude.Num (..)
    , Prelude.Real (..)
    , Prelude.Integral (..)
    , Prelude.Fractional (..)
    , Prelude.Floating (..)
    , Prelude.RealFrac (..)
    , Prelude.RealFloat(..)
      -- ** Data types
    , Prelude.Maybe (..)
    , Prelude.Ordering (..)
    , Prelude.Bool (..)
    , Prelude.Char
    , Prelude.IO
    , Prelude.Either (..)
      -- * Re-exports
      -- ** Packed reps
    , ByteString
    , LByteString
    , Text
    , LText
      -- ** Containers
    , Map
    , HashMap
    , Set
    , HashSet
    , Seq
    , Vector
    , UVector
    , Unbox
    , Hashable
      -- ** Numbers
    , Word
    , Word8
    , Word32
    , Word64
    , Prelude.Int
    , Int32
    , Int64
    , Prelude.Integer
    , Prelude.Rational
    , Prelude.Float
    , Prelude.Double
      -- ** Numeric functions
    , (Prelude.^)
    , (Prelude.^^)
    , Prelude.subtract
    , Prelude.fromIntegral
    , Prelude.realToFrac
      -- ** Monoids
    , Monoid (..)
    , (<>)
      -- ** Arrow
    , Control.Arrow.first
    , Control.Arrow.second
    , (Control.Arrow.***)
    , (Control.Arrow.&&&)
      -- ** Maybe
    , Data.Maybe.mapMaybe
    , Data.Maybe.catMaybes
    , Data.Maybe.fromMaybe
    , Data.Maybe.isJust
    , Data.Maybe.isNothing
    , Data.Maybe.listToMaybe
    , Data.Maybe.maybeToList
      -- ** Either
    , Data.Either.partitionEithers
    , Data.Either.lefts
    , Data.Either.rights
      -- ** Ord
    , Data.Function.on
    , Data.Ord.comparing
    , equating
      -- ** Applicative
    , Control.Applicative.Applicative (..)
    , (Control.Applicative.<$>)
    , (Control.Applicative.<|>)
      -- ** Monad
    , (Control.Monad.>=>)
      -- ** Transformers
    , Control.Monad.Trans.Class.lift
    , Control.Monad.IO.Class.MonadIO
    , Control.Monad.IO.Class.liftIO
      -- ** Exceptions
    , Control.Exception.Exception (..)
    , Data.Typeable.Typeable (..)
    , Control.Exception.SomeException
    , Control.Exception.IOException
    , Control.Exception.Lifted.throwIO
    , Control.Exception.Lifted.try
    , Control.Exception.Lifted.catch
    , Control.Exception.Lifted.handle
    , Control.Exception.Lifted.bracket
    , Control.Exception.Lifted.onException
    , Control.Exception.Lifted.finally
      -- ** Files
    , F.FilePath
    , (F.</>)
    , (F.<.>)
    , F.hasExtension
    , F.basename
    , F.filename
    , F.directory
      -- ** Hashing
    , hash
    , hashWithSalt
      -- ** Print
    , Prelude.print
      -- ** Command line args
    , readArgs
    ) where

import qualified Prelude
import Prelude (Char, (.), Eq, Bool)

import Data.Hashable (Hashable, hash, hashWithSalt)
import Data.Vector.Unboxed (Unbox)

import Data.Monoid (Monoid (..))
import qualified Control.Arrow
import Control.Applicative
import qualified Control.Category
import qualified Control.Monad
import qualified Control.Exception
import qualified Control.Exception.Lifted
import qualified Data.Typeable

import qualified Filesystem.Path.CurrentOS as F

import Data.Word (Word8, Word32, Word64, Word)
import Data.Int (Int32, Int64)

import qualified Data.Text.IO

import qualified Data.Maybe
import qualified Data.Either
import qualified Data.Ord
import qualified Data.Function
import qualified Data.Tuple
import qualified Data.String

import qualified Control.Monad.Trans.Class
import qualified Control.Monad.IO.Class
import Control.Monad.IO.Class (MonadIO (liftIO))

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy
import Data.Text (Text)
import qualified Data.Text.Lazy
import Data.Vector (Vector)
import qualified Data.Vector.Unboxed
import Data.Map (Map)
import Data.Set (Set)
import Data.Sequence (Seq)
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import qualified ReadArgs

import qualified System.Environment
import qualified Data.Text
import qualified Data.List

#if MIN_VERSION_base(4,5,0)
import Data.Monoid ((<>))
#endif

type LText = Data.Text.Lazy.Text
type LByteString = Data.ByteString.Lazy.ByteString
type UVector = Data.Vector.Unboxed.Vector


#if !MIN_VERSION_base(4,5,0)

infixr 6 <>
(<>) :: Monoid w => w -> w -> w
(<>) = mappend
{-# INLINE (<>) #-}

#endif

equating :: Eq a => (b -> a) -> b -> b -> Bool
equating = Data.Function.on (Prelude.==)


getArgs :: MonadIO m => m [Text]
getArgs = liftIO (Data.List.map Data.Text.pack <$> System.Environment.getArgs)

putStrLn :: MonadIO m => Text -> m ()
putStrLn = liftIO . Data.Text.IO.putStrLn

readArgs :: (MonadIO m, ReadArgs.ArgumentTuple a) => m a
readArgs = liftIO ReadArgs.readArgs
