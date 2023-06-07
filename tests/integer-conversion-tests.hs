module Main (main) where

import Test.QuickCheck           ((===))
import Test.QuickCheck.Instances ()
import Test.Tasty                (defaultMain, testGroup)
import Test.Tasty.QuickCheck     (testProperty)

import Data.Integer.Conversion

import qualified Alternative
import qualified Naive

main :: IO ()
main = defaultMain $ testGroup "integer-conversion"
    [ testGroup "text"
        [ testProperty "naive" $ \t -> textToInteger t === Naive.textToInteger t
        , testProperty "alt"   $ \t -> textToInteger t === Alternative.textToInteger t
        ]
    , testGroup "bs"
        [ testProperty "naive" $ \bs -> byteStringToInteger bs === Naive.byteStringToInteger bs
        , testProperty "alt"   $ \bs -> byteStringToInteger bs === Alternative.byteStringToInteger bs
        ]
    ]
