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
eta = 0.05


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
  -- line too fat with <= eta, this can be changed to equality after 2. below
  pointDistance b a + pointDistance a c - pointDistance b c <= 0.001 + threshold


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
mockImage (Polygon ps) = blackIf . flip any (zip ps $ drop 1 ps ++ take 1 ps) . isOnLineFromTo 0
mockImage (ThickPolygon t ps) = blackIf . flip any (zip ps $ drop 1 ps ++ take 1 ps) . isOnLineFromTo t
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
Issues:
  1. Draws one pixel too many
  2. Pixels remain blank if their center point isn't colored in.
     Instead, I should check if the pixel overlaps with a colored point.
  3. After 2., I'll also need to blend color if there's multiple overlaps.
-}
rasterizeMock :: Int -> Int -> Int -> Int -> MockImage -> [[Color]]
rasterizeMock canvasWidth canvasHeight pixelWidth pixelHeight f =
    [ [ fromMaybe white $ f (x, y)
      | x <- interval canvasWidth pixelWidth
      ]
    | y <- map negate $ interval canvasHeight pixelHeight
    ]
  where
    interval :: Int -> Int -> [Double]
    interval dim res =
      let half = fromIntegral dim / 2
          step = half / (fromIntegral res / 2)
      in [-half, -half+step .. half]


display :: [[Color]] -> IO ()
display [] = pure ()
display (row:xs) = do
  putStrLn $ concatMap colToChar row
  display xs
  where
    colToChar c
      | c == black = "#"
      | c == white = "."
      | otherwise = "?"


consoleTest :: Picture -> IO ()
consoleTest p = do
  putStr "use defaults? (y for yes, anything else for no):"
  answer <- getChar
  (a,b,c,d) <- case answer of
    'y' -> putStrLn " " >> pure (10, 10, 200, 200)
    _   -> do
      putStrLn ""
      putStr "canvas height:"
      cX <- readLn
      putStr "canvas width:"
      cY <- readLn
      putStr "pixel width:"
      pX <- readLn
      putStr "pixel height:"
      pY <- readLn
      pure (cX,cY,pX,pY)
  display $ rasterizeMock a b c d $ mockImage p


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
  , con "codeWorldLogo" codeWorldLogo
  , con "coordinatePlane" coordinatePlane
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


instance Observe () [[Color]] Picture where
  -- this takes forever, should probably optimize the rasterizer a bit
  observe () = rasterizeMock 10 10 10 10 . mockImage


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
