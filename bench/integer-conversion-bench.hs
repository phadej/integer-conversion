{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.DeepSeq  (NFData)
import Test.Tasty.Bench (Benchmark, bench, bgroup, defaultMain, nf, whnf)

import qualified Data.ByteString         as BS
import qualified Data.ByteString.Builder as BS.B
import qualified Data.Text               as T
import qualified Data.Text.Lazy.Builder  as T.B

import qualified Alternative
import qualified Naive

import Data.Integer.Conversion

main :: IO ()
main = defaultMain
    [ bgroup "read"
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

        , bgroup "string"
            [ bgroup "naive"  $ seriesL Naive.stringToInteger
            , bgroup "alt"    $ seriesL Alternative.stringToInteger
            , bgroup "read"   $ seriesL read
            , bgroup "proper" $ seriesL stringToInteger
            ]
        ]
    , bgroup "show"
        [ bgroup "string"
            [ bgroup "naive"  $ seriesI Naive.stringFromInteger
            , bgroup "show"   $ seriesI show
            , bgroup "proper" $ seriesI stringFromInteger
            ]
        , bgroup "bytestring"
            [ bgroup "proper" $ seriesI (BS.B.toLazyByteString . bytestringBuilderFromInteger)
            ]
        , bgroup "text"
            [ bgroup "proper" $ seriesI (T.B.toLazyText . textBuilderFromInteger)
            ]
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

    seriesL :: (String -> Integer) -> [Benchmark]
    seriesL f =
        [ bench (show n) $ whnf f t
        | e <- [6 .. 18 :: Int]
        , let n = 2 ^ e
        , let t = replicate n '9'
        ]

    seriesI :: NFData a => (Integer -> a) -> [Benchmark]
    seriesI f =
        [ bench (show n) $ nf f t
        | e <- [6 .. 18 :: Int]
        , let n = 2 ^ e
        , let t = read (replicate n '9') :: Integer
        ]
