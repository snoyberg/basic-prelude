{-# LANGUAGE NoImplicitPrelude #-}
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
    , Prelude.zip
    , Prelude.unzip
    , Prelude.zipWith
    , Prelude.or
    , Data.Text.IO.putStrLn
    , Prelude.elem
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
    , Prelude.Enum (..)
    , Prelude.Show
    , Prelude.Functor (..)
    , Prelude.Monad (..)
    , (Control.Monad.=<<)
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
    , empty
    , concat
    , (++)
      -- ** Arrow
    , Control.Arrow.first
    , Control.Arrow.second
    , (Control.Arrow.***)
    , (Control.Arrow.&&&)
      -- ** Maybe
    , Data.Maybe.mapMaybe
    , Data.Maybe.catMaybes
    , Data.Maybe.fromMaybe
      -- ** Either
    , Data.Either.partitionEithers
      -- ** Ord
    , Data.Function.on
    , Data.Ord.comparing
    , equating
      -- ** Applicative
    , Control.Applicative.Applicative (..)
    , (Control.Applicative.<$>)
      -- ** Monad
    , (Control.Monad.>=>)
      -- ** Transformers
    , Control.Monad.Trans.Class.lift
    , Control.Monad.IO.Class.MonadIO
    , Control.Monad.IO.Class.liftIO
      -- ** Exceptions
    , Control.Exception.Exception (..)
    , Control.Exception.SomeException
    , Control.Exception.throwIO
      -- ** Files
    , F.FilePath
    , (F.</>)
    , (F.<.>)
    , F.hasExtension
    , F.basename
    , F.filename
      -- ** Print
    , Prelude.print
    ) where

import qualified Prelude
import Prelude (Char, (.), Eq, Bool)

import Data.Hashable (Hashable)
import Data.Vector.Unboxed (Unbox)

import Data.Monoid (Monoid (..))
import qualified Control.Arrow
import qualified Control.Applicative
import qualified Control.Category
import qualified Control.Monad
import qualified Control.Exception

import qualified Filesystem.Path.CurrentOS as F

import Data.Word (Word8, Word32, Word64, Word)
import Data.Int (Int32, Int64)

import qualified Data.Text.IO

import qualified Data.Maybe
import qualified Data.Either
import qualified Data.Ord
import qualified Data.Function
import qualified Data.Tuple

import qualified Control.Monad.Trans.Class
import qualified Control.Monad.IO.Class

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy
import Data.Text (Text)
import qualified Data.Text.Lazy
import Data.Vector (Vector)
import qualified Data.Vector.Unboxed
import Data.Map (Map)
import Data.Set (Set)
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)

type LText = Data.Text.Lazy.Text
type LByteString = Data.ByteString.Lazy.ByteString
type UVector = Data.Vector.Unboxed.Vector

empty :: Monoid w => w
empty = mempty

concat :: Monoid w => [w] -> w
concat = mconcat

infixr 5  ++
(++) :: Monoid w => w -> w -> w
(++) = mappend

equating :: Eq a => (b -> a) -> b -> b -> Bool
equating = Data.Function.on (Prelude.==)
