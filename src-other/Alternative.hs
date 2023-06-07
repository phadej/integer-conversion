{-# LANGUAGE BangPatterns #-}
-- | A sub-quadratic algorithm for conversion of digits into 'Integer'.
-- Pairs of adjacent radix @b@ digits are combined into a single radix @b^2@ digit.
-- This process is repeated until we are left with a single digit.
-- This algorithm performs well only on large inputs,
-- so we use the simple algorithm for smaller inputs.
--
-- This implementation is taken from aeson-2.1.
module Alternative (
    byteStringToInteger,
    textToInteger,
) where

import Data.Char (ord)

import qualified Data.Text as T
import qualified Data.ByteString as BS

byteStringToInteger :: BS.ByteString -> Integer
byteStringToInteger bs
    -- here (and similarly in 'textToInteger') it could make sense
    -- to do first loop directly on 'ByteString' (or 'Text'),
    -- but as this is already a slow path, we opt rather for a simpler implementation.
    | l > 40    = valInteger' 10 l [ fromIntegral w - 48 | w <- BS.unpack bs ]
    | otherwise = byteStringToIntegerSimple bs
  where
    !l = BS.length bs

byteStringToIntegerSimple :: BS.ByteString -> Integer
byteStringToIntegerSimple = BS.foldl' step 0 where
  step a b = a * 10 + fromIntegral b - 48

textToInteger :: T.Text -> Integer
textToInteger bs
    | l > 40    = valInteger' 10 l [ fromIntegral (ord w - 48) | w <- T.unpack bs ]
    | otherwise = textToIntegerSimple bs
  where
    !l = T.length bs

textToIntegerSimple :: T.Text -> Integer
textToIntegerSimple = T.foldl' step 0 where
  step a b = a * 10 + fromIntegral (ord b - 48) -- 48 = '0'

-- | A sub-quadratic algorithm.
--
-- Call 'valInteger'' directly if you know length of @digits@ in advance.
-- valInteger :: Integer -> [Integer] -> Integer
-- valInteger base ds = valInteger' base (length ds) ds

-- | A sub-quadratic algorithm implementation.
valInteger'
    :: Integer    -- ^ base
    -> Int        -- ^ length of digits
    -> [Integer]  -- ^ digits
    -> Integer
valInteger' = go
  where
    go :: Integer -> Int -> [Integer] -> Integer
    go _ _ []  = 0
    go _ _ [d] = d
    go b l ds
        | l > 40 = b' `seq` go b' l' (combine b ds')
        | otherwise = valIntegerSimple b ds
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
valIntegerSimple :: Integer -> [Integer] -> Integer
valIntegerSimple base = go 0
  where
    go r [] = r
    go r (d : ds) = r' `seq` go r' ds
      where
        r' = r * base + fromIntegral d

