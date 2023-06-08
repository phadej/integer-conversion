module Main (main) where

import Test.QuickCheck           ((===))
import Test.QuickCheck.Instances ()
import Test.Tasty                (defaultMain, testGroup)
import Test.Tasty.QuickCheck     (testProperty, label)

import qualified Data.ByteString as BS
import qualified Data.Text       as T

import Data.Integer.Conversion

import qualified Alternative
import qualified Naive

main :: IO ()
main = defaultMain $ testGroup "integer-conversion"
    [ testGroup "text"
        [ testProperty "naive" $ \t -> labelT t $ textToInteger t === Naive.textToInteger t
        , testProperty "alt"   $ \t -> labelT t $ textToInteger t === Alternative.textToInteger t
        ]
    , testGroup "bs"
        [ testProperty "naive" $ \bs -> labelB bs $ byteStringToInteger bs === Naive.byteStringToInteger bs
        , testProperty "alt"   $ \bs -> labelB bs $ byteStringToInteger bs === Alternative.byteStringToInteger bs
        ]
    , testGroup "string"
        [ testProperty "naive" $ \s -> labelS s $ stringToInteger s === Naive.stringToInteger s
        , testProperty "alt"   $ \s -> labelS s $ stringToInteger s === Alternative.stringToInteger s
        ]
    ]
  where
    labelT t = label (if T.length t  >= 40 then "long" else "short")
    labelB b = label (if BS.length b >= 40 then "long" else "short")
    labelS s = label (if length s    >= 40 then "long" else "short")
