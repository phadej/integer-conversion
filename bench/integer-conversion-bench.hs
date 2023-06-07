{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Tasty.Bench (Benchmark, bench, bgroup, defaultMain, whnf)

import qualified Data.ByteString as BS
import qualified Data.Text       as T

import qualified Alternative
import qualified Naive

import Data.Integer.Conversion

main :: IO ()
main = defaultMain
    [ bgroup "text"
        [ bgroup "naive"  $ seriesT Naive.textToInteger
        , bgroup "alt"    $ seriesT Alternative.textToInteger
        , bgroup "proper" $ seriesT textToInteger
        ]

    , bgroup "bytestring"
        [ bgroup "naive"  $ seriesB Naive.byteStringToInteger
        , bgroup "alt"    $ seriesB Alternative.byteStringToInteger
        , bgroup "proper" $ seriesB byteStringToInteger
        ]
    ]
  where
    seriesT :: (T.Text -> Integer) -> [Benchmark]
    seriesT f =
        [ bench (show n) $ whnf f t
        | e <- [6 .. 18 :: Int]
        , let n = 2 ^ e
        , let t = T.replicate n "9"
        ]

    seriesB :: (BS.ByteString -> Integer) -> [Benchmark]
    seriesB f =
        [ bench (show n) $ whnf f t
        | e <- [6 .. 18 :: Int]
        , let n = 2 ^ e
        , let t = BS.replicate n (48 + 9)
        ]
