{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module DeriveLaws where


import Control.Monad (guard)
import Data.Functor (($>))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import QuickSpec
import Test.QuickCheck
import Test.QuickCheck.Arbitrary ()
import CodeWorld hiding (ReifyPicture(..))

import CodeWorld.Test


type MockImage = Point -> Maybe Color


eta :: Double
eta = 0.01


blackIf :: Bool -> Maybe Color
blackIf condition = guard condition $> black


mockCircle :: Double -> Double -> MockImage
mockCircle (abs -> radius) (abs -> threshold) (x,y) = blackIf $
  abs (sqrt (x^(2 :: Int) + y^(2 :: Int)) - radius) <= threshold + eta



mockRectangle :: Bool -> Double -> Double -> Double -> MockImage
mockRectangle filled (abs -> w) (abs -> h) (abs -> threshold) (abs -> x, abs -> y) =
    blackIf $
      preventInnerPoints &&
      x <= w/2 + threshold/2 + eta && y <= h/2 + threshold/2 + eta
  where
    preventInnerPoints =
      filled ||
      abs (y - h/2) <= threshold/2 + eta ||
      abs (x - w/2) <= threshold/2 + eta


composeImages :: MockImage -> MockImage -> MockImage
composeImages f g pt = case f pt of
  Nothing    -> g pt
  first@(Just color) -> case alpha color of
    1 -> first
    0 -> g pt
    _ -> do
      case g pt of
        Nothing -> pure color
        Just color2 -> pure $ mixed [color, color2]


pointDistance :: Point -> Point -> Double
pointDistance a b = vectorLength $ vectorDifference a b


isOnLineFromTo :: Double -> Point -> (Point, Point) -> Bool
isOnLineFromTo threshold a (b,c) =
  -- line too fat with <= eta
  pointDistance b a + pointDistance a c - pointDistance b c  <= 0.001 + threshold


mockImage :: Picture -> MockImage
mockImage Blank = const Nothing
-- don't care about these for testing,
-- but what should I use here to avoid spurious laws?
mockImage Logo = const Nothing
mockImage CoordinatePlane = const Nothing
mockImage (Circle r) = mockCircle r 0
mockImage (ThickCircle t r) = mockCircle r t
mockImage (SolidCircle r) = mockCircle 0 r
mockImage (Rectangle w h) = mockRectangle False w h 0
mockImage (ThickRectangle t w h) = mockRectangle False w h t
mockImage (SolidRectangle w h) = mockRectangle True w h 0
mockImage (Polyline ps) = blackIf . flip any (zip ps $ drop 1 ps) . isOnLineFromTo 0
mockImage (ThickPolyline t ps) = blackIf . flip any (zip ps $ drop 1 ps) . isOnLineFromTo t
mockImage (Polygon ps)
  | length ps == 1 = const Nothing
  | otherwise = blackIf . flip any (zip ps $ drop 1 ps ++ take 1 ps) . isOnLineFromTo 0
mockImage (ThickPolygon t ps)
  | length ps == 1 = const Nothing
  | otherwise = blackIf . flip any (zip ps $ drop 1 ps ++ take 1 ps) . isOnLineFromTo t
-- doesn't draw anything in some cases, e.g solidPolygon [(1,0),(1,1), (2,0)]...
mockImage (SolidPolygon ps) = blackIf . flip isInsidePolygon ps
mockImage (Color c p) = (c <$) . mockImage p
mockImage (Translate x y p) = mockImage p . translatedPoint (-x) (-y)
mockImage (Rotate a p) = mockImage p . rotatedPoint (-a)
mockImage (Reflect a p) = mockImage p . reflectedPoint (-a)
mockImage (Clip x y p) = \pt@(a,b) -> do
  guard $ abs a <= abs x/2 + eta && abs b <= abs y/2 + eta
  mockImage p pt
-- i/0 = Infinity => empty image checks out!?
mockImage (Scale fac1 fac2 p) = mockImage p . scaledPoint (1/fac1) (1/fac2)
mockImage (Dilate fac p) = mockImage p . dilatedPoint (1/fac)
mockImage (And p q) = composeImages (mockImage p) (mockImage q)
mockImage (Pictures xs) = foldr (composeImages . mockImage) (const Nothing) xs
mockImage _ = const Nothing


{-
Issue: terribly slow!
computing `image` multiple times slows down everything.
-}
rasterizeMock :: Double -> Double -> Int -> Int -> Int -> MockImage -> [[Color]]
rasterizeMock viewportWidth viewportHeight resWidth resHeight samplesPerAxis image =
    [ [ rasterizePixel col row
      | col <- [0 .. resWidth - 1]
      ]
    | row <- [0 .. resHeight - 1]
    ]
  where
    pixelWidth = viewportWidth / fromIntegral resWidth
    pixelHeight = viewportHeight / fromIntegral resHeight

    rasterizePixel col row = averageColors
      [ image (x, y)
      | let startX = -viewportWidth / 2 + fromIntegral col * pixelWidth
      , let startY =  viewportHeight / 2 - fromIntegral row * pixelHeight
      , x <- samplesBetween startX (startX + pixelWidth)
      , y <- samplesBetween (startY - pixelHeight) startY
      ]

    samplesBetween start end =
      let step = (end - start) / fromIntegral samplesPerAxis
      in [ start + step * (fromIntegral subPixel + 0.5)
         | subPixel <- [0 .. samplesPerAxis - 1]
         ]

    -- slow and probably not correct in all cases!
    -- this needs to be evaluated to a real color,
    -- but that is even slower?
    averageColors xs = mixed $ map (fromMaybe white) xs

display :: [[Color]] -> IO ()
display = mapM_ $ putStrLn . unwords . map colToChar
  where
    observeColor = observe ()
    colToChar c
      | colorValues == observeColor black = "#"
      | colorValues == observeColor white = "."
      | otherwise = "?"
      where colorValues = observeColor c


consoleTest :: Picture -> IO ()
consoleTest p = do
  putStr "use defaults? (y for yes, anything else for no):"
  answer <- getChar
  (a,b,c,d,e) <- case answer of
    'y' -> putStrLn " " >> pure (10, 10, 124, 124, 3)
    _   -> do
      putStrLn ""
      putStr "viewport height:"
      cX <- readLn
      putStr "viewport width:"
      cY <- readLn
      putStr "pixel width:"
      pX <- readLn
      putStr "pixel height:"
      pY <- readLn
      putStr "samples per axis per pixel (super sampling):"
      s <- readLn
      pure (cX,cY,pX,pY,s)
  display $ rasterizeMock a b c d e $ mockImage p


sig :: Sig
sig = signature
  [ con "rectangle" rectangle
  , con "thickRectangle" thickRectangle
  , con "solidRectangle" solidRectangle
  , con "circle" circle
  , con "thickCircle" thickCircle
  , con "solidCircle" solidCircle
  --, con "arc" arc
  --, con "thickArc" thickArc
  --, con "sector" sector
  , con "blank" blank
--  , con "codeWorldLogo" codeWorldLogo
--  , con "coordinatePlane" coordinatePlane
  --, con "lettering" lettering
  --, con "styledLettering" styledLettering
  , con "polyline" polyline
  , con "thickPolyline" thickPolyline
  , con "polygon" polygon
  , con "thickPolygon" thickPolygon
  , con "solidPolygon" solidPolygon
  --, con "curve" curve
  --, con "thickCurve" thickCurve
  --, con "closedCurve" closedCurve
  --, con "thickClosedCurve" thickClosedCurve
  --, con "solidClosedCurve" solidClosedCurve
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
  , mono @[Picture]
  , monoObserve @Color
  , mono @Text
  , mono @TextStyle
  , mono @Font
  -- Is that even useful? Turns of warnings, but laws can never use generated functions?
  , inst (Sub Dict :: Arbitrary A :- Arbitrary (Picture -> A))
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
  , con "*" $ (*) @Double
--  , con "-" $ (-) @Double
--  , con "negate" $ negate @Double
  , lists
  , con "map" $ map @A @B
  ]


instance Observe
  ( Positive Double
  , Positive Double
  , Positive Int
  , Positive Int
  )
  [[Color]]
  Picture where
  -- this takes forever, should probably optimize the rasterizer a bit
  observe (getPositive -> a, getPositive -> b, getPositive -> c, getPositive -> d)
    = rasterizeMock a b c d 3 . mockImage


instance Observe () (Double,Double,Double,Double) Color where
  -- this takes forever, should probably optimize the rasterizer a bit
  observe () c = (hue c, saturation c, luminosity c, alpha c)


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
--  , (1, pure codeWorldLogo)
--  , (1, pure coordinatePlane)
  , (2, rectangle <$> arbitrary <*> arbitrary)
  , (2, thickRectangle <$> positiveDouble <*> arbitrary <*> arbitrary)
  , (2, solidRectangle <$> arbitrary <*> arbitrary)
  , (2, circle <$> arbitrary)
  , (2, solidCircle <$> arbitrary)
  , (2, uncurry thickCircle <$> validThicknessRatio)
  --, (2, arc <$> arbitrary <*> arbitrary <*> arbitrary)
  --, (2, thickArc <$> positiveDouble <*> arbitrary <*> arbitrary <*> arbitrary)
  --, (2, sector <$> arbitrary <*> arbitrary <*> arbitrary)
  --, (2, lettering <$> arbitrary)
  --, (2, styledLettering <$> arbitrary <*> arbitrary <*> arbitrary)
  , (2, polyline <$> arbitrary)
  , (2, thickPolyline <$> positiveDouble <*> arbitrary)
  , (2, polygon <$> arbitrary)
  , (2, thickPolygon <$> positiveDouble <*> arbitrary)
  , (2, solidPolygon <$> arbitrary)
  --, (2, curve <$> arbitrary)
  --, (2, thickCurve <$> positiveDouble <*> arbitrary)
  --, (2, closedCurve <$> arbitrary)
  --, (2, thickClosedCurve <$> positiveDouble <*> arbitrary)
  --, (2, solidClosedCurve <$> arbitrary)
  ]


instance CoArbitrary Picture where
  -- should probably add a real implementation
  -- if this is required for something after all!
  coarbitrary p = coarbitrary (show p)


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


isLeftOfLine :: Point -> (Point,Point) -> Bool
isLeftOfLine (xP,yP) ((x1,y1),(x2,y2)) =
  ((x2 - x1) * (yP - y1)) - ((xP - x1) * (y2 - y1)) > 0

isInsidePolygon :: Point -> [Point] -> Bool
isInsidePolygon p@(_,y) ps =
    foldr windingNumber 0 (zip ps $ drop 1 ps ++ take 1 ps) > 0
  where
    windingNumber :: (Point, Point) -> Int -> Int
    windingNumber line@((_,y1),(_,y2)) acc
      | y1 <= y && (y2 > y) && isLeftOfLine p line = acc + 1
      | y2 <= y && not (isLeftOfLine p line) = acc - 1
      | otherwise = acc



main :: IO ()
main = quickSpec $ sig <> sigTypes <> sigBg
