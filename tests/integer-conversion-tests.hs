{-# OPTIONS_GHC -Wno-orphans #-}
module Main (main) where

import Data.Char             (chr, ord)
import Test.QuickCheck       ((===))
import Test.Tasty            (defaultMain, testGroup)
import Test.Tasty.QuickCheck (Arbitrary (..), counterexample, label, testProperty)

import qualified Data.ByteString as BS
import qualified Data.Text       as T

import Data.Integer.Conversion

import qualified Alternative
import qualified Naive

main :: IO ()
main = defaultMain $ testGroup "integer-conversion"
    [ testGroup "text"
        [ testProperty "naive" $ \t' -> let t = nts t' in labelT t $ textToInteger t === Naive.textToInteger t
        , testProperty "alt"   $ \t' -> let t = nts t' in labelT t $ textToInteger t === Alternative.textToInteger t
        ]
    , testGroup "bytestring"
        [ testProperty "naive" $ \bs' -> let bs = nbs bs' in labelB bs $ counterexample (show bs) $ byteStringToInteger bs === Naive.byteStringToInteger bs
        , testProperty "alt"   $ \bs' -> let bs = nbs bs' in labelB bs $ counterexample (show bs) $ byteStringToInteger bs === Alternative.byteStringToInteger bs
        ]
    , testGroup "string"
        [ testProperty "naive" $ \s' -> let s = filter (<'\xFF') s' in labelS s $ stringToInteger s === Naive.stringToInteger s
        , testProperty "alt"   $ \s' -> let s = filter (<'\xFF') s' in labelS s $ stringToInteger s === Alternative.stringToInteger s
        ]
    ]
  where
    labelT t = label (if T.length t  >= 40 then "long" else "short")
    labelB b = label (if BS.length b >= 40 then "long" else "short")
    labelS s = label (if length s    >= 40 then "long" else "short")

-------------------------------------------------------------------------------
-- Normalisation of inputs
-------------------------------------------------------------------------------

-- make bytestring of only 0..9 characters.
nbs :: BS.ByteString -> BS.ByteString
nbs = BS.map (\w -> 48 + rem w 10)

nts :: T.Text -> T.Text
nts = T.map $ \c -> chr $ 48 + rem (ord c) 10

-------------------------------------------------------------------------------
-- Orphans
-------------------------------------------------------------------------------

-- we could use quickcheck-instances,
-- but by defining these instances here we make adopting newer GHC smoother.

instance Arbitrary T.Text where
    arbitrary = fmap T.pack arbitrary

instance Arbitrary BS.ByteString where
    arbitrary = fmap BS.pack arbitrary
