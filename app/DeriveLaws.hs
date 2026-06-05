{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module DeriveLaws where


import Control.Monad (guard)
import Data.Fixed (mod')
import Data.Functor (($>))
import Data.List (zip4)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Diagrams.Solve.Polynomial (cubForm, quadForm)
import QuickSpec
import Test.QuickCheck
import Test.QuickCheck.Arbitrary ()
import CodeWorld hiding (ReifyPicture(..))

import CodeWorld.Test


type MockImage = Point -> Maybe Color


epsilon :: Double
epsilon = 0.01

(.^) :: Num a => a -> Integer -> a
(.^) = (^)


blackIf :: Bool -> Maybe Color
blackIf condition = guard condition $> black


mockCircle :: Double -> Double -> MockImage
mockCircle = mockArc 0 (2*pi)


mockArc :: Double -> Double -> Double -> Double -> MockImage
mockArc start end radius (abs -> threshold) (x,y)
  | start == end = Nothing
  | otherwise = blackIf $
  abs (sqrt (x*x + y*y) - radius) <= threshold + epsilon && inArc
  where
    full = 2*pi
    modStart = start `mod'` full
    modEnd = end `mod'` full
    lowerBound = min modStart modEnd
    upperBound = max modStart modEnd
    angle = atan2 y x `mod'` (2*pi)

    inArc
      | abs (end - start) >= full = True
      -- the direction of the line stroke changes based on this...
      | signum start + signum end `elem` [1,-2,2] =
          angle <= upperBound && angle >= lowerBound
      | otherwise =
          angle >= upperBound || angle <= lowerBound


mockRectangle :: Bool -> Double -> Double -> Double -> MockImage
mockRectangle filled w h (abs -> threshold) (abs -> x, abs -> y) =
    blackIf $
      preventInnerPoints &&
      x <= halfW + bound && y <= halfH + bound
  where
    halfW = w/2
    halfH = h/2
    bound = threshold/2 + epsilon
    preventInnerPoints =
      filled ||
      abs (y - halfH) <= bound ||
      abs (x - halfW) <= bound


mockCurve :: Double -> [Point] -> MockImage
mockCurve threshold [p1,p2] = blackIf . flip (isOnLineFromTo threshold) (p1,p2)
mockCurve threshold ps@(p1:p2:p3:_) = \p -> blackIf $
  -- quadratic for first segment
  onQuadBezier threshold p (p1,computeControlPoint (p1,p2,p3),p2) ||
  -- then cubic for inner segments
  let zipMonster = zip4 ps (drop 1 ps) (drop 2 ps) (drop 3 ps)
      (e3,e2,e1) = unsafeTakeLastThree ps
  in any (\(a,b,c,d) -> let (c1,c2) = computeControlPointsMiddle (a,b,c,d)
          in onCubicBezier threshold p (b,c1,c2,c)
        ) zipMonster ||
  -- and quadratic again for the last segment
  onQuadBezier threshold p (e2,computeControlPointEnd (e1,e2,e3),e3)
mockCurve _ _ = const Nothing


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
  -- line too fat with <= epsilon
  pointDistance b a + pointDistance a c - pointDistance b c  <= 0.001 + threshold


mockImage :: Picture -> MockImage
mockImage Blank = const Nothing
mockImage Logo = const Nothing
mockImage CoordinatePlane = const Nothing
mockImage (Circle r) = mockCircle r 0
mockImage (ThickCircle t r) = mockCircle r t
mockImage (SolidCircle r) = mockCircle 0 r
mockImage (Rectangle w h) = mockRectangle False w h 0
mockImage (ThickRectangle t w h) = mockRectangle False w h t
mockImage (SolidRectangle w h) = mockRectangle True w h 0
mockImage (Arc start end r) = mockArc start end r 0
mockImage (ThickArc t start end r) = mockArc start end r t
mockImage (Sector start end r) = mockArc start end 0 r
mockImage (Polyline ps) = blackIf . flip any (zip ps $ drop 1 ps) . isOnLineFromTo 0
mockImage (ThickPolyline t ps) = blackIf . flip any (zip ps $ drop 1 ps) . isOnLineFromTo t
mockImage (Polygon ps)
  | length ps == 1 = const Nothing
  | otherwise = blackIf . flip any (zip ps $ drop 1 ps ++ take 1 ps) . isOnLineFromTo 0
mockImage (ThickPolygon t ps)
  | length ps == 1 = const Nothing
  | otherwise = blackIf . flip any (zip ps $ drop 1 ps ++ take 1 ps) . isOnLineFromTo t
mockImage (SolidPolygon ps) = blackIf . flip isInsidePolygon ps
mockImage (Curve xs) = mockCurve 0 xs
mockImage (ThickCurve t xs) = mockCurve t xs
mockImage (Color c p) = (c <$) . mockImage p
mockImage (Translate x y p) = mockImage p . translatedPoint (-x) (-y)
mockImage (Rotate a p) = mockImage p . rotatedPoint (-a)
mockImage (Reflect a p) = mockImage p . reflectedPoint (-a)
mockImage (Clip x y p) = \pt@(a,b) -> do
  guard $ abs a <= abs x/2 + epsilon && abs b <= abs y/2 + epsilon
  mockImage p pt
-- i/0 = Infinity => empty image checks out!?
mockImage (Scale fac1 fac2 p) = mockImage p . scaledPoint (1/fac1) (1/fac2)
mockImage (Dilate fac p) = mockImage p . dilatedPoint (1/fac)
mockImage (And p q) = composeImages (mockImage p) (mockImage q)
mockImage (Pictures xs) = foldr (composeImages . mockImage) (const Nothing) xs
mockImage _ = const Nothing

unsafeTakeLastThree :: [a] -> (a,a,a)
unsafeTakeLastThree xs = case reverse xs of
  (a1:a2:a3:_) -> (a1,a2,a3)
  _            -> error "not enough elements"



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
  , con "arc" arc
  , con "thickArc" thickArc
  , con "sector" sector
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
  , con "curve" curve
  , con "thickCurve" thickCurve
  --, con "closedCurve" closedCurve
  --, con "thickClosedCurve" thickClosedCurve
  --, con "solidClosedCurve" solidClosedCurve
  , con "translated" translated
  , con "scaled" scaled
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
  , vars ["x","y"] $ Proxy @Double
  , vars ["s"] $ Proxy @Style
  , vars ["f"] $ Proxy @Font
  , vars ["c"] $ Proxy @Color
  , vars ["p"] $ Proxy @Picture
  , vars ["ps"] $ Proxy @[Picture]
  , vars ["pts"] $ Proxy @[Point]
  , withMaxTestSize 10
  , withPrintStyle ForHumans
  , withPruningTermSize 9
  ]

sigBg :: Sig
sigBg = background
  [ arith $ Proxy @Double
  , con "*" $ (*) @Double
--  , con "-" $ (-) @Double
--  , con "negate" $ negate @Double
--  , con @Double "2*pi" (2*pi)
--  , con @Double "pi" pi
  , lists
  , con "translatedPoint" translatedPoint
  , con "scaledPoint" scaledPoint
  , con "reflectedPoint" reflectedPoint
  , con "rotatedPoint" rotatedPoint
  , con "foldr" $ foldr @[] @Picture @Picture
  , con "map" $ map @A @B
  ]

-- TODO: this produces useless images far too often.
-- I need to write a generator for these values,
-- such that they strike a good balance between amount of pixels and subpixels.
instance Observe
  ( Positive Double
  , Positive Double
  , Positive Int
  , Positive Int
  )
  [[Color]]
  Picture where
  -- this takes forever, should probably optimize the rasterizer a bit
  observe (Positive a, Positive b, Positive c, Positive d)
    = rasterizeMock a b c d 3 . mockImage


instance Observe () (Double,Double,Double,Double) Color where
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
  , (2, rectangle <$> nonNegative <*> nonNegative)
  , (2, thickRectangle <$> nonNegative <*> nonNegative <*> nonNegative)
  , (2, solidRectangle <$> nonNegative <*> nonNegative)
  , (2, circle <$> nonNegative)
  , (2, solidCircle <$> nonNegative)
  , (2, uncurry thickCircle <$> validThicknessRatio)
  , (2, arc <$> arbitrary <*> arbitrary <*> nonNegative)
  , (2, thickArc <$> nonNegative <*> arbitrary <*> arbitrary <*> nonNegative)
  , (2, sector <$> arbitrary <*> arbitrary <*> nonNegative)
  --, (2, lettering <$> arbitrary)
  --, (2, styledLettering <$> arbitrary <*> arbitrary <*> arbitrary)
  , (2, polyline <$> arbitrary)
  , (2, thickPolyline <$> nonNegative <*> arbitrary)
  , (2, polygon <$> arbitrary)
  , (2, thickPolygon <$> nonNegative <*> arbitrary)
  , (2, solidPolygon <$> arbitrary)
  , (2, curve <$> arbitrary)
  , (2, thickCurve <$> nonNegative <*> arbitrary)
  --, (2, closedCurve <$> arbitrary)
  --, (2, thickClosedCurve <$> positiveDouble <*> arbitrary)
  --, (2, solidClosedCurve <$> arbitrary)
  ]
  where
    nonNegative = getNonNegative <$> arbitrary

decayArbitrary :: Arbitrary a => Int -> Gen a
decayArbitrary n = scale (`div` n) arbitrary


pictureList :: Gen [Picture]
pictureList = do
  m <- getSize
  k  <- chooseInt (0, m)
  vectorOf k (resize (m-k) arbitrary)


validThicknessRatio :: Gen (Double, Double)
validThicknessRatio = do
  size <- getNonNegative <$> arbitrary
  thickness <- choose (0,size*2)
  pure (thickness, size)


isLeftOfLine :: Point -> (Point,Point) -> Bool
isLeftOfLine (xP,yP) ((x1,y1),(x2,y2)) =
  ((x2 - x1) * (yP - y1)) - ((xP - x1) * (y2 - y1)) > 0

isInsidePolygon :: Point -> [Point] -> Bool
isInsidePolygon p@(_,y) ps =
    foldr windingNumber 0 (zip ps $ drop 1 ps ++ take 1 ps) /= 0
  where
    windingNumber :: (Point, Point) -> Int -> Int
    windingNumber line@((_,y1),(_,y2)) acc
      | y1 <= y && (y2 > y) && isLeftOfLine p line = acc + 1
      | y2 <= y && y1 > y && not (isLeftOfLine p line) = acc - 1
      | otherwise = acc


onBezier :: Double -> (Double, Double) -> t -> t -> (t -> Double -> [Double]) -> (t -> Double -> Double) -> Bool
onBezier threshold (pX,pY) pointsX pointsY howToGetRoots theCurve = any isOnIt candidates
  where
    candidates = howToGetRoots pointsX pX ++ howToGetRoots pointsY pY
    isOnIt t = t >= 0 && t <= 1 &&
      abs (theCurve pointsX t - pX) <= epsilon + threshold &&
      abs (theCurve pointsY t - pY) <= epsilon + threshold


onQuadBezier :: Double -> Point -> (Point,Point,Point) -> Bool
onQuadBezier threshold point ((startX, startY), (controlX, controlY), (endX, endY)) =
    onBezier threshold point (startX, controlX, endX) (startY, controlY, endY) rootsForCoord quad
  where
    quad (start, control, end) t =
      (1 - t).^2 * start +
      2 * (1 - t) * t * control +
      t.^2 * end

    rootsForCoord (start, control, end) p = quadForm a b c
      where
        a = start - 2 * control + end
        b = 2 * (control - start)
        c = start - p


onCubicBezier :: Double -> Point -> (Point,Point,Point,Point) -> Bool
onCubicBezier threshold point ((startX, startY), (controlX1, controlY1), (controlX2, controlY2), (endX, endY)) =
    onBezier threshold point (startX, controlX1, controlX2, endX) (startY, controlY1, controlY2, endY) rootsForCoord cubic
  where
    cubic (start, control1, control2, end) t =
      (1 - t).^3 * start +
      2 * (1 - t).^2 * t * control1 +
      3 * (1 - t) * t.^2 * control2 +
      t.^3 * end

    rootsForCoord (start, control1, control2, end) p = cubForm a b c d
      where
        a = -start + 3*control1 - 3*control2 + end
        b = 3*start - 6*control1 + 3*control2
        c = -3*start + 3*control1
        d = start - p


computeControlPoint :: (Point,Point,Point) -> Point
computeControlPoint (p1@(x1,y1),p2@(x2,y2),p3@(x3,y3)) = (x,y)
  where
    x = x2 + ratio * (x1-x3) / 2
    y = y2 + ratio * (y1-y3) / 2
    distStart = pointDistance p1 p2
    distEnd = pointDistance p2 p3
    ratio = distStart / (distStart + distEnd)


computeControlPointsMiddle :: (Point,Point,Point,Point) -> (Point,Point)
computeControlPointsMiddle (p1@(x1,y1),p2@(x2,y2),p3@(x3,y3),p4@(x4,y4)) = ((cx1,cy1),(cx2,cy2))
  where
    cx1 = x2 + ratio2 * (x3-x1) / 2
    cy1 = y2 + ratio2 * (y3 - y1) / 2
    cx2 = x3 + ratio1 * (x2 - x4) / 2
    cy2 = y3 + ratio1 * (y2 - y4) / 2
    distStart = pointDistance p1 p2
    distMiddle = pointDistance p2 p3
    distEnd = pointDistance p3 p4
    ratio1 = distMiddle / (distMiddle + distEnd)
    ratio2 = distMiddle / (distStart + distMiddle)


computeControlPointEnd :: (Point,Point,Point) -> Point
computeControlPointEnd (p1@(x1,y1),p2@(x2,y2),p3@(x3,y3)) = (x,y)
  where
    x = x2 + ratio * (x3-x1) / 2
    y = y2 + ratio * (y3-y1) / 2
    distStart = pointDistance p2 p3
    distEnd = pointDistance p1 p2
    ratio = distStart / (distStart + distEnd)



main :: IO ()
main = quickSpec $ sig <> sigTypes <> sigBg
