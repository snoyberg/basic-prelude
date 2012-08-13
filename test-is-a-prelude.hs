{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

import qualified BasicPrelude as P

type Bool = P.Bool
_True = P.True
_False = P.False
(&&) = (P.&&)
(||) = (P.||)
not = P.not
otherwise = P.otherwise

type Maybe = P.Maybe
_Nothing = P.Nothing
_Just = P.Just
maybe = P.maybe

type Either = P.Either
_Left = P.Left
_Right = P.Right
either = P.either

type Ordering = P.Ordering
_LT = P.LT
_GT = P.GT
_EQ = P.EQ

type Char = P.Char
type String = P.String

fst = P.fst
snd = P.snd
curry = P.curry
uncurry = P.uncurry

(==) = (P.==)
(/=) = (P./=)

compare = P.compare
(<) = (P.<)
(>=) = (P.>=)
(>) = (P.>)
(<=) = (P.<=)
max = P.max
min = P.min

succ = P.succ
pred = P.pred
toEnum = P.toEnum
fromEnum = P.fromEnum
enumFrom = P.enumFrom
enumFromThen = P.enumFromThen
enumFromTo = P.enumFromTo
enumFromThenTo = P.enumFromThenTo

minBound = P.minBound
maxBound = P.maxBound

type Int = P.Int
type Integer = P.Integer
type Float = P.Float
type Double = P.Double
type Rational = P.Rational

(+) = (P.+)
(*) = (P.*)
(-) = (P.-)
negate = P.negate
abs = P.abs
signum = P.signum
fromInteger = P.fromInteger

toRational = P.toRational

quot = P.quot
rem = P.rem
div = P.div
mod = P.mod
quotRem = P.quotRem
divMod = P.divMod
toInteger = P.toInteger

(/) = (P./)
recip = P.recip
fromRational = P.fromRational

pi = P.pi
exp = P.exp
sqrt = P.sqrt
log = P.log
(**) = (P.**)
logBase = P.logBase
sin = P.sin
cos = P.cos
tan = P.tan
asin = P.asin
atan = P.atan
acos = P.acos
sinh = P.sinh
tanh = P.tanh
cosh = P.cosh
asinh = P.asinh
atanh = P.atanh
acosh = P.acosh

properFraction = P.properFraction
truncate = P.truncate
round = P.round
ceiling = P.ceiling
floor = P.floor

floatRadix = P.floatRadix
floatDigits = P.floatDigits
floatRange = P.floatRange
decodeFloat = P.decodeFloat
encodeFloat = P.encodeFloat
exponent = P.exponent
significand = P.significand
scaleFloat = P.scaleFloat
isNaN = P.isNaN
isInfinite = P.isInfinite
isDenormalized = P.isDenormalized
isNegativeZero = P.isNegativeZero
isIEEE = P.isIEEE
atan2 = P.atan2

subtract = P.subtract
even = P.even
odd = P.odd
gcd = P.gcd
lcm = P.lcm
(^) = (P.^)
(^^) = (P.^^)
fromIntegral = P.fromIntegral
realToFrac = P.realToFrac

(>>=) = (P.>>=)
(>>) = (P.>>)
return = P.return
fail = P.fail

fmap = P.fmap

mapM = P.mapM
mapM_ = P.mapM_
sequence = P.sequence
sequence_ = P.sequence_
(=<<) = (P.=<<)

id = P.id
const = P.const
(.) = (P..)
flip = P.flip
($) = (P.$)
until = P.until
asTypeOf = P.asTypeOf
error = P.error
undefined = P.undefined
seq = P.seq
($!) = (P.$!)

map = P.map
(++) = (P.++)
filter = P.filter
head = P.head
last = P.last
tail = P.tail
init = P.init
null = P.null
length = P.length
(!!) = (P.!!)
reverse = P.reverse

foldl = P.foldl
foldl1 = P.foldl1
foldr = P.foldr
foldr1 = P.foldr1

and = P.and
or = P.or
any = P.any
all = P.all
sum = P.sum
product = P.product
concat = P.concat
concatMap = P.concatMap
maximum = P.maximum
minimum = P.minimum

scanl = P.scanl
scanl1 = P.scanl1
scanr = P.scanr
scanr1 = P.scanr1

iterate = P.iterate
repeat = P.repeat
replicate = P.replicate
cycle = P.cycle

take = P.take
drop = P.drop
splitAt = P.splitAt
takeWhile = P.takeWhile
dropWhile = P.dropWhile
span = P.span
break = P.break

elem = P.elem
notElem = P.notElem
lookup = P.lookup

zip = P.zip
zip3 = P.zip3
zipWith = P.zipWith
zipWith3 = P.zipWith3
unzip = P.unzip
unzip3 = P.unzip3

lines = P.lines
words = P.words
unlines = P.unlines
unwords = P.unwords

type ShowS = P.ShowS
showsPrec = P.showsPrec
show = P.show
showList = P.showList

shows = P.shows
showChar = P.showChar
showString = P.showString
showParen = P.showParen

type ReadS a = P.ReadS a
readsPrec = P.readsPrec
readList = P.readList

reads = P.reads
readParen = P.readParen
read = P.read
lex = P.lex

type IO a = P.IO a
putChar = P.putChar
putStr = P.putStr
putStrLn = P.putStrLn
print = P.print

getChar = P.getChar
getLine = P.getLine
getContents = P.getContents
interact = P.interact

type FilePath = P.FilePath
readFile = P.readFile
writeFile = P.writeFile
appendFile = P.appendFile
readIO = P.readIO
readLn = P.readLn

type IOError = P.IOError
ioError = P.ioError
userError = P.userError
-- catch = P.catch -- deprecated

