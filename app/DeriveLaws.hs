{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
module DeriveLaws where


import Data.Maybe (fromMaybe)
import Data.Text (Text)
import QuickSpec
import Test.QuickCheck
import Test.QuickCheck.Arbitrary ()
import CodeWorld hiding (ReifyPicture(..))

import CodeWorld.Test


type MockImage = Point -> Maybe Color


mockImage :: Picture -> MockImage
mockImage Blank _ = Nothing
mockImage (Circle r) (x,y) = if abs (sqrt (x^2 + y^2) - r) <= 0.01 then Just black else Nothing
mockImage _ _ = Nothing


rasterizeMock :: Int -> Int -> MockImage -> [Color]
rasterizeMock width height f = [ fromMaybe white $ f  (x, y) | x <- [-fromIntegral width/2.. fromIntegral width/2], y <- [-fromIntegral height/2.. fromIntegral height/2]]

display :: [[Color]] -> IO ()
display [] = pure ()
display (row:xs) = do
  putStrLn $ concatMap colToChar row
  display xs
  where
    colToChar c
      | c == black = "#"
      | c == white = " "
      | otherwise = "?"


sig :: Sig
sig = signature
  [ con "rectangle" rectangle
  , con "thickRectangle" thickRectangle
  , con "solidRectangle" solidRectangle
  , con "circle" circle
  , con "thickCircle" thickCircle
  , con "solidCircle" solidCircle
  , con "arc" arc
  , con "thickArc" thickArc
  , con "sector" sector
  , con "blank" blank
  , con "codeWorldLogo" codeWorldLogo
  , con "coordinatePlane" coordinatePlane
  , con "lettering" lettering
  , con "styledLettering" styledLettering
  , con "polyline" polyline
  , con "thickPolyline" thickPolyline
  , con "polygon" polygon
  , con "thickPolygon" thickPolygon
  , con "solidPolygon" solidPolygon
  , con "curve" curve
  , con "thickCurve" thickCurve
  , con "closedCurve" closedCurve
  , con "thickClosedCurve" thickClosedCurve
  , con "solidClosedCurve" solidClosedCurve
  , con "translated" translated
  , con "scaled" scaled
  , con "dilated" dilated
  , con "colored" colored
  , con "rotated" rotated
  , con "reflected" reflected
  , con "clipped" clipped
  , con "&" (&)
  , con "pictures" pictures
  ]

sigTypes :: Sig
sigTypes = signature
  [ monoObserve @Picture
  , monoObserve @[Picture]
  , mono @Color
  , mono @Text
  , mono @TextStyle
  , mono @Font
  , vars ["x","y"] $ Proxy @Double
  , vars ["s"] $ Proxy @Style
  , vars ["f"] $ Proxy @Font
  , vars ["c"] $ Proxy @Color
  , vars ["p"] $ Proxy @Picture
  , vars ["ps"] $ Proxy @[Picture]
  , vars ["pts"] $ Proxy @[Point]
  , withInferInstanceTypes
  ]

sigBg :: Sig
sigBg = background
  [ arith $ Proxy @Double
--  , lists
 -- , con "*" $ (*) @Double
 -- , con "-" $ (-) @Double
 -- , con "negate" $ negate @Double
  ]


instance Observe () Picture Picture where
  observe () = normalize


instance Arbitrary Color where
  arbitrary = do
    n <- arbitrary `suchThat` (>=0)
    pure $ assortedColors !! n


instance Arbitrary TextStyle where
  arbitrary = elements
    [ Plain
    , Bold
    , Italic
    ]


instance Arbitrary Font where
  arbitrary = elements
    [ SansSerif
    , Serif
    , Monospace
    , Handwriting
    , Fancy
    , NamedFont "Font"
    ]


instance Arbitrary Picture where
  arbitrary = sized $ \n ->
    if n <= 1
      then basic
      else frequency
        [ (1, translated <$> arbitrary <*> arbitrary <*> decayArbitrary 2)
        , (1, scaled <$> arbitrary <*> arbitrary <*> decayArbitrary 2)
        , (1, dilated <$> arbitrary <*> decayArbitrary 2)
        , (1, colored <$> arbitrary <*> decayArbitrary 2)
        , (1, rotated <$> arbitrary <*> decayArbitrary 2)
        , (1, reflected <$> arbitrary <*> decayArbitrary 2)
        , (1, clipped <$> arbitrary <*> arbitrary <*> decayArbitrary 2)
        , (1, (&) <$> decayArbitrary 2 <*> decayArbitrary 2)
        , (2, pictures <$> pictureList)
        ]


basic :: Gen Picture
basic = frequency
  [ (1, pure blank)
  , (1, pure codeWorldLogo)
  , (1, pure coordinatePlane)
  , (2, rectangle <$> arbitrary <*> arbitrary)
  , (2, thickRectangle <$> positiveDouble <*> arbitrary <*> arbitrary)
  , (2, solidRectangle <$> arbitrary <*> arbitrary)
  , (2, circle <$> arbitrary)
  , (2, solidCircle <$> arbitrary)
  , (2, uncurry thickCircle <$> validThicknessRatio)
  , (2, arc <$> arbitrary <*> arbitrary <*> arbitrary)
  , (2, thickArc <$> positiveDouble <*> arbitrary <*> arbitrary <*> arbitrary)
  , (2, sector <$> arbitrary <*> arbitrary <*> arbitrary)
  , (2, lettering <$> arbitrary)
  , (2, styledLettering <$> arbitrary <*> arbitrary <*> arbitrary)
  , (2, polyline <$> arbitrary)
  , (2, thickPolyline <$> positiveDouble <*> arbitrary)
  , (2, polygon <$> arbitrary)
  , (2, thickPolygon <$> positiveDouble <*> arbitrary)
  , (2, solidPolygon <$> arbitrary)
  , (2, curve <$> arbitrary)
  , (2, thickCurve <$> positiveDouble <*> arbitrary)
  , (2, closedCurve <$> arbitrary)
  , (2, thickClosedCurve <$> positiveDouble <*> arbitrary)
  , (2, solidClosedCurve <$> arbitrary)
  ]


decayArbitrary :: Arbitrary a => Int -> Gen a
decayArbitrary n = scale (`div` n) arbitrary


pictureList :: Gen [Picture]
pictureList = do
  m <- getSize
  k  <- chooseInt (0, m)
  vectorOf k (resize (m-k) arbitrary)


positiveDouble :: Gen Double
positiveDouble = arbitrary `suchThat` (>=0)

validThicknessRatio :: Gen (Double, Double)
validThicknessRatio = do
  size <- arbitrary
  thickness <- choose (0,abs size*2)
  pure (thickness, size)


main :: IO ()
main = quickSpec $ sig <> sigTypes <> sigBg
