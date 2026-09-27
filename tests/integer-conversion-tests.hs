{-# OPTIONS_GHC -Wno-orphans #-}
module Main (main) where

import Data.Char             (chr, ord)
import Test.QuickCheck       ((===))
import Test.Tasty            (defaultMain, testGroup)
import Test.Tasty.QuickCheck (Arbitrary (..), counterexample, label, testProperty)

import qualified Data.ByteString            as BS
import qualified Data.ByteString.Builder    as BS.B
import qualified Data.ByteString.Lazy.Char8 as LBS8
import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as LT
import qualified Data.Text.Lazy.Builder     as T.B

import Data.Integer.Conversion

import qualified Alternative
import qualified Naive

main :: IO ()
main = defaultMain $ testGroup "integer-conversion"
    [ testGroup "read"
        [ testGroup "text"
            [ testProperty "naive" $ \t' -> let t = nts t' in labelT t $ textToInteger t === Naive.textToInteger t
            , testProperty "alt"   $ \t' -> let t = nts t' in labelT t $ textToInteger t === Alternative.textToInteger t
            ]
        , testGroup "bs"
            [ testProperty "naive" $ \bs' -> let bs = nbs bs' in labelB bs $ counterexample (show bs) $ byteStringToInteger bs === Naive.byteStringToInteger bs
            , testProperty "alt"   $ \bs' -> let bs = nbs bs' in labelB bs $ counterexample (show bs) $ byteStringToInteger bs === Alternative.byteStringToInteger bs
            ]
        , testGroup "string"
            [ testProperty "naive" $ \s' -> let s = filter (<'\xFF') s' in labelS s $ stringToInteger s === Naive.stringToInteger s
            , testProperty "alt"   $ \s' -> let s = filter (<'\xFF') s' in labelS s $ stringToInteger s === Alternative.stringToInteger s
            ]
        ]
    , testGroup "show"
        [ testGroup "string"
            [ testProperty "naive" $ \i -> showsFromInteger i "" === Naive.stringFromInteger i
            , testProperty "base"  $ \i -> showsFromInteger i "" === show i
            ]
        , testGroup "bs"
            [ testProperty "base" $ \i -> BS.B.toLazyByteString (bytestringBuilderFromInteger i) === LBS8.pack (show i)
            ]
        , testGroup "text"
            [ testProperty "base" $ \i -> T.B.toLazyText (textBuilderFromInteger i) === LT.pack (show i)
            ]
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
