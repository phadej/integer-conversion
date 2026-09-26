{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -ddump-simpl -dsuppress-all -ddump-to-file #-}
-- | The naive left fold to convert digits to integer is quadratic
-- as multiplying (big) 'Integer's is not a constant time operation.
--
-- This module provides sub-quadratic algorithm for conversion of 'Text'
-- or 'ByteString' into 'Integer'.
--
-- For example for a text of 262144 9 digits, fold implementation
-- takes 1.5 seconds, and 'textToInteger' just 26 milliseconds on my machine.
-- Difference is already noticeable around 100-200 digits.
--
-- In particular 'read' is correct (i.e. faster) than @List.foldl'@ (better complexity),
-- 'stringToInteger' is a bit faster than 'read' (same complexity, lower coeffcient).
--
module Data.Integer.Conversion (
    -- * To Integer
    textToInteger,
    byteStringToInteger,
    stringToInteger,
    stringToIntegerWithLen,
    -- * From Integer
    showsFromInteger,
    bytestringBuilderFromInteger,
    textBuilderFromInteger,
) where

import Control.Monad.ST     (ST, runST)
import Data.ByteString      (ByteString)
import Data.Char            (ord)
import Data.Int             (Int64)
import Data.Primitive.Array (MutableArray, newArray, readArray, writeArray)
import Data.Text.Internal   (Text (..))
import Data.Word            (Word8)

import qualified Data.ByteString            as BS
import qualified Data.ByteString.Builder    as BS.B
import qualified Data.List                  as L
import qualified Data.Text                  as T
import qualified Data.Text.Array            as A
import qualified Data.Text.Lazy.Builder     as T.B
import qualified Data.Text.Lazy.Builder.Int as T.B

-- $setup
-- >>> :set -XOverloadedStrings

pattern Digits :: Int
pattern Digits = 18

pattern Base :: Integer
pattern Base = 1_000_000_000_000_000_000

-------------------------------------------------------------------------------
-- Text To
-------------------------------------------------------------------------------

-- | Convert 'Text' to 'Integer'.
--
-- Semantically same as @T.foldl' (\acc c -> acc * 10 + toInteger (ord c - 48)) 0@,
-- but this is more efficient.
--
-- >>> textToInteger "123456789"
-- 123456789
--
-- For non-decimal inputs some nonsense is calculated
--
-- >>> textToInteger "foobar"
-- 6098556
--
textToInteger :: Text -> Integer
textToInteger t@(Text _arr _off len)
    -- len >= 20000 = algorithmL 10 (T.length t) [ toInteger (ord c - 48) | c <- T.unpack t ]
    | len >= 40    = complexTextToInteger t
    | otherwise    = simpleTextToInteger t

simpleTextToInteger :: Text -> Integer
simpleTextToInteger = T.foldl' (\acc c -> acc * 10 + fromChar c) 0

complexTextToInteger :: Text -> Integer
complexTextToInteger (Text input off len) = runST $ do
    arr <- newArray len' 0

    if r == 0
    then go arr 0 0 0 0
    else goPfx arr 0 0 0
  where
    (q, r) = quotRem len Digits
    len' = if r > 0 then q + 1 else q

    indexArray :: Int -> Int64
    indexArray i = fromIntegral (A.unsafeIndex input (off + i) - 48)

    goPfx :: MutableArray s Integer -> Int -> Int -> Int64 -> ST s Integer
    goPfx !arr !i !n !acc
          -- this cannot happen as r is less than or equal to len.
--        | i >= len
--        = do
--            return integer0
--            writeArray arr 0 (toInteger acc)
--            algorithm arr len' Base

        | n >= r
        = do
            writeArray arr 0 (toInteger acc)
            go arr i 1 0 0

        | otherwise
        = do
            goPfx arr (i + 1) (n + 1) (acc * 10 + indexArray i)

    go :: MutableArray s Integer -> Int -> Int -> Int -> Int64 -> ST s Integer
    go !arr !i !o !n !acc
        | i >= len
        = do
            writeArray arr o (toInteger acc)
            algorithm arr len' Base

        | n >= Digits
        = do
            writeArray arr o (toInteger acc)
            go arr i (o + 1) 0 0

        | otherwise
        = do
            go arr (i + 1) o (n + 1) (acc * 10 + indexArray i)

fromChar :: Char -> Integer
fromChar c = toInteger (ord c - 48 :: Int)
{-# INLINE fromChar #-}

fromChar' :: Char -> Int64
fromChar' c = fromIntegral (ord c - 48 :: Int)
{-# INLINE fromChar' #-}

-------------------------------------------------------------------------------
-- ByteString To
-------------------------------------------------------------------------------

-- | Convert 'ByteString' to 'Integer'.
--
-- Semantically same as @BS.foldl' (\acc c -> acc * 10 + toInteger c - 48) 0@,
-- but this is more efficient.
--
-- >>> byteStringToInteger "123456789"
-- 123456789
--
-- For non-decimal inputs some nonsense is calculated
--
-- >>> byteStringToInteger "foobar"
-- 6098556
--
byteStringToInteger :: ByteString -> Integer
byteStringToInteger bs
    -- len >= 20000 = algorithmL 10 len [ toInteger w - 48 | w <- BS.unpack bs ]
    | len >= 40    = complexByteStringToInteger len bs
    | otherwise    = simpleByteStringToInteger bs
  where
    !len = BS.length bs

simpleByteStringToInteger :: BS.ByteString -> Integer
simpleByteStringToInteger = BS.foldl' (\acc w -> acc * 10 + toInteger (fromWord8 w)) 0

complexByteStringToInteger :: Int -> BS.ByteString -> Integer
complexByteStringToInteger len bs = runST $ do
    arr <- newArray len' 0

    if r == 0
    then go arr 0 0 0 0
    else goPfx arr 0 0 0
  where
    (q, r) = quotRem len Digits
    len' = if r > 0 then q + 1 else q

    goPfx :: MutableArray s Integer -> Int -> Int -> Int64 -> ST s Integer
    goPfx !arr !i !n !acc
          -- this cannot happen as r is less than or equal to len.
--        | i >= len
--        = do
--            return integer0
--            writeArray arr 0 (toInteger acc)
--            algorithm arr len' Base

        | n >= r
        = do
            writeArray arr 0 (toInteger acc)
            go arr i 1 0 0

        | otherwise
        = do
            goPfx arr (i + 1) (n + 1) (acc * 10 + indexBS bs i)

    go :: MutableArray s Integer -> Int -> Int -> Int -> Int64 -> ST s Integer
    go !arr !i !o !n !acc
        | i >= len
        = do
            writeArray arr o (toInteger acc)
            algorithm arr len' Base

        | n >= Digits
        = do
            writeArray arr o (toInteger acc)
            go arr i (o + 1) 0 0

        | otherwise
        = do
            go arr (i + 1) o (n + 1) (acc * 10 + indexBS bs i)

indexBS :: BS.ByteString -> Int -> Int64
indexBS bs i = fromIntegral (fromWord8 (BS.index bs i))
{-# INLINE indexBS #-}

fromWord8 :: Word8 -> Int
fromWord8 w = fromIntegral w - 48
{-# INLINE fromWord8 #-}

-------------------------------------------------------------------------------
-- String To
-------------------------------------------------------------------------------

-- | Convert 'String' to 'Integer'.
--
-- Semantically same as @List.foldl' (\acc c -> acc * 10 + toInteger c - 48) 0@,
-- but this is more efficient.
--
-- >>> stringToInteger "123456789"
-- 123456789
--
-- For non-decimal inputs some nonsense is calculated
--
-- >>> stringToInteger "foobar"
-- 6098556
--
stringToInteger :: String -> Integer
stringToInteger str = stringToIntegerWithLen str (length str)

-- | Convert 'String' to 'Integer' when you know the length beforehand.
--
-- >>> stringToIntegerWithLen "123" 3
-- 123
--
-- If the length is wrong, you may get wrong results.
-- (Simple algorithm is used for short strings which ignores the length
-- argument).
--
-- >>> stringToIntegerWithLen (replicate 40 '0' ++ "123") 45
-- 123
--
-- >>> stringToIntegerWithLen (replicate 40 '0' ++ "123") 44
-- 123
--
-- >>> stringToIntegerWithLen (replicate 40 '0' ++ "123") 42
-- 12
--
stringToIntegerWithLen :: String -> Int -> Integer
stringToIntegerWithLen str len
    | len >= 40    = complexStringToInteger len str
    | otherwise    = simpleStringToInteger str

simpleStringToInteger :: String -> Integer
simpleStringToInteger = L.foldl' step 0 where
  step a b = a * 10 + fromChar b

-- See https://github.com/ghc/ghc/commit/a5a4c25626e11e8b4be6687a9af8cfc85a77e9ba
--
-- This is further improved algorithm:
--
-- - In the first iteration we group up to 18 digits (such numbers fit into 64 bit Int so multiplication stays constant-time operation).
--   This doesn't improve algorithmic complexity, but it makes algorithm run faster.
-- - And then we begin to pair adjacent digits
-- - We also use MutableArray to avoid allocating list cons cells.
--
complexStringToInteger :: Int -> String -> Integer
complexStringToInteger len str = runST $ do
    arr <- newArray len' integer0
    if r == 0
    then go arr 0 0 0 str
    else goPfx arr 0 0 str
  where
    (q, r) = quotRem len Digits
    len' = if r > 0 then q + 1 else q

    goPfx :: MutableArray s Integer -> Int -> Int64 -> String -> ST s Integer
    goPfx !_arr !_n !_acc [] = return integer0 -- this shouldn't happen, but may if len is wrong.
    goPfx !arr !n !acc input | n >= r = do
        writeArray arr 0 (toInteger acc)
        go arr 1 0 0 input
    goPfx !arr !n !acc (d:ds) =
        goPfx arr (n + 1) (acc * 10 + fromChar' d) ds

    go :: MutableArray s Integer -> Int -> Int -> Int64 -> String -> ST s Integer
    go !arr !o !_n !acc [] = do
        writeArray arr o (toInteger acc)
        algorithm arr len' Base
    go !arr !o !n !acc input | n >= Digits = do
        writeArray arr o (toInteger acc)
        go arr (o + 1) 0 0 input
    go !arr !o !n !acc (d:ds) = do
        go arr o (n + 1) (acc * 10 + fromChar' d) ds

-------------------------------------------------------------------------------
-- Algorithm
-------------------------------------------------------------------------------

-- The core of algorithm uses mutable arrays.
-- An alternative (found in e.g. @base@) uses lists.
-- For very big integers (thousands of decimal digits) the difference
-- is small (runtime is dominated by integer multiplication),
-- but for medium sized integers this is slightly faster, as we avoid cons cell allocation.
--
algorithm
    :: forall s. MutableArray s Integer  -- ^ working buffer
    -> Int                               -- ^ buffer size
    -> Integer                           -- ^ base
    -> ST s Integer
algorithm !arr !len !base
    | len <= 40 = finish 0 0
    | even len  = loop 0 0
    | otherwise = loop 1 1
  where
    loop :: Int -> Int -> ST s Integer
    loop !i !o | i < len = do
        -- read at i, i +1
        a <- readArray arr i
        b <- readArray arr (i + 1)

        -- rewrite with constant to release memory
        writeArray arr i       integer0
        writeArray arr (i + 1) integer0

        -- write at o
        writeArray arr o $! a * base + b

        -- continue
        loop (i + 2) (o + 1)

    loop _ _ = algorithm arr len' base'
      where
        !base' = base * base
        !len'  = (len + 1) `div` 2

    finish :: Integer -> Int -> ST s Integer
    finish !acc !i | i < len = do
        a <- readArray arr i
        finish (acc * base + a) (i + 1)
    finish !acc !_ =
        return acc

-------------------------------------------------------------------------------
-- List variant
-------------------------------------------------------------------------------

{-

-- | A sub-quadratic algorithm implementation using lists.
--
-- Sometimes this is faster, but I fail to quantify when exactly.
--
algorithmL
    :: Integer    -- ^ base
    -> Int        -- ^ length of digits
    -> [Integer]  -- ^ digits
    -> Integer
algorithmL = go
  where
    go :: Integer -> Int -> [Integer] -> Integer
    go _ _ []  = 0
    go _ _ [d] = d
    go b l ds
        | l > 40 = b' `seq` go b' l' (combine b ds')
        | otherwise = finishAlgorithmL b ds
      where
        -- ensure that we have an even number of digits
        -- before we call combine:
        ds' = if even l then ds else 0 : ds
        b' = b * b
        l' = (l + 1) `quot` 2

    combine b (d1 : d2 : ds) = d `seq` (d : combine b ds)
      where
        d = d1 * b + d2
    combine _ []  = []
    combine _ [_] = errorWithoutStackTrace "this should not happen"

-- | The following algorithm is only linear for types whose Num operations
-- are in constant time.
--
-- We export this (mostly) for testing purposes.
--
finishAlgorithmL :: Integer -> [Integer] -> Integer
finishAlgorithmL base = go 0
  where
    go r [] = r
    go r (d : ds) = r' `seq` go r' ds
      where
        r' = r * base + fromIntegral d
-}

-------------------------------------------------------------------------------
-- Misc
-------------------------------------------------------------------------------

integer0 :: Integer
integer0 = 0

-------------------------------------------------------------------------------
-- String From
-------------------------------------------------------------------------------

-- | Convert 'Integer' to a decimal 'String'.
--
-- The naive approach is to print one digit at a time using 'quotRem',
-- but this is more efficient.
--
-- >>> showsFromInteger 123456789 ""
-- "123456789"
--
-- @since 0.1.2
--
showsFromInteger :: Integer -> ShowS
showsFromInteger = shows

-------------------------------------------------------------------------------
-- ByteString From
-------------------------------------------------------------------------------

-- |
--
-- @since 0.1.2
--
bytestringBuilderFromInteger :: Integer -> BS.B.Builder
bytestringBuilderFromInteger = BS.B.integerDec

-------------------------------------------------------------------------------
-- Text From
-------------------------------------------------------------------------------

-- |
--
-- @since 0.1.2
--
textBuilderFromInteger :: Integer -> T.B.Builder
textBuilderFromInteger = T.B.decimal
