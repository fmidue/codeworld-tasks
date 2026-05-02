{-# OPTIONS_GHC -Wno-orphans #-}
module DeriveLaws where


import QuickSpec
import Test.QuickCheck
import Test.QuickCheck.Arbitrary ()
import CodeWorld
import CodeWorld.Test (normalize)


sig :: Sig
sig = signature
  [ con "rectangle" rectangle
  , con "circle" circle
  , con "arc" arc
  , con "blank" blank
  , con "translated" $ translated
  , con "scaled" $ scaled
  , con "colored" colored
  ]

sigTypes :: Sig
sigTypes = signature
  [ monoObserve @Picture
  , mono @Color
  , vars ["x","y"] $ Proxy @Double
  , vars ["c"] $ Proxy @Color
  , vars ["p"] $ Proxy @Picture
  ]

sigBg :: Sig
sigBg = background
  [ arith $ Proxy @Double
  , con "*" $ (*) @Double
  , con "-" $ (-) @Double
  , con "negate" $ negate @Double
  ]



instance Observe () Picture Picture where
  observe () t = normalize t


instance Arbitrary Color where
  arbitrary = do
    n <- arbitrary `suchThat` (>=0)
    pure $ assortedColors !! n


instance Arbitrary Picture where
  arbitrary = sized $ \n ->
    if n <= 1
      then pure blank
      else frequency
        [ (1, pure blank)
        , (2, rectangle <$> arbitrary <*> arbitrary)
        , (2, circle <$> arbitrary)
        , (2, arc <$> arbitrary <*> arbitrary <*> arbitrary)
        , (1, translated <$> arbitrary <*> arbitrary <*> decayArbitrary 2)
        , (1, scaled <$> arbitrary <*> arbitrary <*> decayArbitrary 2)
        , (1, colored <$> arbitrary <*> decayArbitrary 2)
        ]

decayArbitrary :: Arbitrary a => Int -> Gen a
decayArbitrary n = scale (`div` n) arbitrary


main :: IO ()
main = quickSpec $ sig <> sigTypes <> sigBg
