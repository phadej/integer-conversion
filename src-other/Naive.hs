module Naive (
    textToInteger,
    byteStringToInteger,
) where

import Data.Char (ord)

import qualified Data.ByteString as BS
import qualified Data.Text       as T

textToInteger :: T.Text -> Integer
textToInteger = T.foldl' (\acc c -> acc * 10 + toInteger (ord c - 48)) 0

byteStringToInteger :: BS.ByteString -> Integer
byteStringToInteger = BS.foldl' (\acc c -> acc * 10 + toInteger c - 48) 0
