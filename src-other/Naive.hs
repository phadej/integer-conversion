module Naive (
    textToInteger,
    byteStringToInteger,
    stringToInteger,
    stringFromInteger,
) where

import Data.Char (ord, chr)

import qualified Data.ByteString as BS
import qualified Data.List       as L
import qualified Data.Text       as T

textToInteger :: T.Text -> Integer
textToInteger = T.foldl' (\acc c -> acc * 10 + toInteger (ord c - 48)) 0

byteStringToInteger :: BS.ByteString -> Integer
byteStringToInteger = BS.foldl' (\acc c -> acc * 10 + toInteger c - 48) 0

stringToInteger :: String -> Integer
stringToInteger = L.foldl' (\acc c -> acc * 10 + toInteger (ord c - 48)) 0

stringFromInteger :: Integer -> String
stringFromInteger i0 = case compare i0 0 of
    LT -> '-' : go (negate i0) ""
    EQ -> "0"
    GT -> go i0 ""
  where
    go :: Integer -> ShowS
    go i = if i <= 0 then id else let (q, r) = quotRem i 10 in go q . showChar (chr (fromInteger r + 48))
