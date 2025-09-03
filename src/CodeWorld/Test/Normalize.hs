{-# language DeriveDataTypeable #-}
{-# language FlexibleInstances #-}
{-# language OverloadedStrings #-}
{-# language ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module CodeWorld.Test.Normalize (
  NormalizedPicture(..),
  contains,
  couldHaveTranslation,
  count,
  getColor,
  getRotation,
  getExactRotation,
  getScalingFactors,
  getExactScalingFactors,
  getTranslation,
  getExactTranslation,
  getReflectionAngle,
  getExactReflectionAngle,
  getCircleRadius,
  getExactCircleRadius,
  getRectangleLengths,
  getExactRectangleLengths,
  getExactPointList,
  getSubPictures,
  stripTranslation,
  toConcretePicture,
  ) where


import Data.Data                        (Data,Typeable)
import Data.Text                        (Text)
import Data.List.Extra                  (headDef, takeEnd)
import Data.Maybe                       (listToMaybe)
import Data.Tuple.Extra                 (both)
import Data.Generics.Uniplate.Data      (transform, universe)

import CodeWorld.Tasks.API              (Drawable(..))
import CodeWorld.Tasks.VectorSpace (
  Point,
  vectorSum,
  atOriginWithOffset,
  isRectangle,
  reflectedPoint,
  rotationAngle,
  scaledVector,
  sideLengths,
  rotatedVector,
  )
import CodeWorld.Test.AbsTypes

import qualified CodeWorld.Tasks.Picture as P


{- |
A more abstract syntax tree representing images.
Comparisons between values of this type are intentionally fuzzy:
Concrete number or point values are abstracted into coarser categories.
Notably, those values are not lost and can be retrieved if desired.

The constructors of this type are not exposed.
Values are built using the CodeWorld API.
-}
data NormalizedPicture
  = Rectangle !ShapeKind !Size !Size
  | Circle !ShapeKind !Size
  | Lettering !Text
  | Color !AbsColor !NormalizedPicture
  | Translate !Position !Position !NormalizedPicture
  | Scale !Factor !Factor !NormalizedPicture
  | Rotate !Angle !NormalizedPicture
  | Pictures [NormalizedPicture]
  | CoordinatePlane
  | Logo
  | Blank
  | Polyline !ShapeKind [AbsPoint]
  | Curve !ShapeKind [AbsPoint]
  | Arc !ShapeKind !Angle !Angle !Size
  | Reflect !Angle !NormalizedPicture
  | Clip !Size !Size !NormalizedPicture
  deriving (Show,Eq,Ord,Data,Typeable)


instance Drawable NormalizedPicture where

  pictures = foldr (&) Blank

  Blank & p = p
  p & Blank = p
  Polyline (Hollow Normal) ps1 & Polyline (Hollow Thick) ps2 =
    Polyline (Hollow Thick) ps2 & Polyline (Hollow Normal) ps1
  Polyline (Hollow t) ps1 & Polyline Solid ps2 =
    Polyline Solid ps2 & Polyline (Hollow t) ps1
  Curve (Hollow Normal) ps1 & Curve (Hollow Thick) ps2 =
    Curve (Hollow Thick) ps2 & Curve (Hollow Normal) ps1
  Curve (Hollow t) ps1 & Curve Solid ps2 =
    Curve Solid ps2 & Curve (Hollow t) ps1
  Polyline sp ps1 & Curve sc ps2 = Curve sc ps2 & Polyline sp ps1
  Polyline s1 ps1 & Polyline s2 ps2 = handleFreeShape True  s1 s2 ps1 ps2
  Curve    s1 ps1 & Curve    s2 ps2 = handleFreeShape False s1 s2 ps1 ps2
  p & Polyline s ps = Polyline s ps & p
  p & Curve s ps = Curve s ps & p
  p1 & p2 = Pictures $ ps1 ++ ps2
    where
      ps1 = case p1 of
        Pictures ps -> ps
        _           -> [p1]
      ps2 = case p2 of
        Pictures ps -> ps
        _           -> [p2]

  blank = Blank

  coordinatePlane = CoordinatePlane
  codeWorldLogo = Logo

  circle 0 = blank
  circle r = Circle (Hollow Normal) $ toSize r

  solidCircle 0 = blank
  solidCircle r = Circle Solid $ toSize r

  thickCircle 0 _ = blank
  thickCircle (validThickness -> t) (abs -> r)
    | t <= 2 * r = Circle shape $ toSize (r + t/2)
    | otherwise = error $
        "The line width of a thickCircle must not be greater than the diameter. " ++
        "(This error was thrown inside the test suite)"
    where
      shape
        | t == 2*r = Solid
        | otherwise = Hollow $ thickness t

  rectangle 0 _ = blank
  rectangle _ 0 = blank
  rectangle l w = toWideRectangle (Hollow Normal) l w

  solidRectangle 0 _ = blank
  solidRectangle _ 0 = blank
  solidRectangle l w = toWideRectangle Solid l w

  thickRectangle _ 0 _ = blank
  thickRectangle _ _ 0 = blank
  thickRectangle (validThickness -> t) (abs -> l) (abs -> w) =
      toWideRectangle shape (l + t/2) (w + t/2)
    where
      shape
        | t >= 2*l || t >= 2*w = Solid
        | otherwise = Hollow $ thickness t

  arc      = checkForCircle $ Hollow Normal
  sector   = checkForCircle Solid
  thickArc (validThickness -> t) = checkForCircle $ Hollow $ thickness t

  curve            = handlePointList $ Curve $ Hollow Normal
  thickCurve (validThickness -> t) = handlePointList $ Curve $ Hollow $ thickness t
  solidClosedCurve = handlePointList (Curve Solid) . toOpenShape

  closedCurve        = curve . toOpenShape
  thickClosedCurve (validThickness -> t) = thickCurve t . toOpenShape

  polyline        = checkForRectangle $ Hollow Normal
  thickPolyline (validThickness -> t) = checkForRectangle $ Hollow $ thickness t
  solidPolygon    = checkForRectangle Solid . toOpenShape

  polygon        = polyline . toOpenShape
  thickPolygon (validThickness -> t) = thickPolyline t . toOpenShape

  lettering "" = blank
  lettering t  = Lettering t

  styledLettering _ _ "" = blank
  styledLettering _ _ t = Lettering t

  translated 0 0 p = p
  translated x y p = case p of
    Translate a b q -> translated (x + fromPosition a) (y + fromPosition b) q
    Pictures ps     -> Pictures $ map (translated x y) ps
    Blank           -> Blank
    Color c q       -> Color c $ translated x y q
    Polyline s ps   -> Polyline s $ map (applyToAbsPoint (vectorSum (x,y))) ps
    Curve s ps      -> Curve    s $ map (applyToAbsPoint (vectorSum (x,y))) ps
    a               -> Translate (toPosition x) (toPosition y) a

  colored c p = case p of
    Color _ q      -> colored c q
    Pictures ps    -> Pictures $ map (colored c) ps
    Blank          -> Blank
    q              -> case toAbsColor c of
      Tone 0 0 0 -> q
      absC       -> Color absC q

  dilated fac = scaled fac fac

  scaled 0 _ _ = blank
  scaled _ 0 _ = blank
  scaled 1 1 p = p
  scaled fac1 fac2 (Circle sk s) | fac1 == fac2 =
    Circle sk (toSize $ fromSize s *fac1)
  scaled fac1 fac2 (Rectangle sk s1 s2) =
    shapeKindToRectangle sk (fromSize s1 *fac1) (fromSize s2 *fac2)
  scaled fac1 fac2 p = case p of
    Scale f1 f2 q    -> scaled (fromFactor f1 * fac1) (fromFactor f2 * fac2) q
    Translate x y q  -> Translate
      (toPosition $ fromPosition x*fac1)
      (toPosition $ fromPosition y*fac2)
      $ scaled fac1 fac2 q
    Blank            -> Blank
    Color c q        -> Color c $ scaled fac1 fac2 q
    Pictures ps      -> Pictures $ map (scaled fac1 fac2) ps
    Polyline s ps    -> Polyline s $ map (applyToAbsPoint (scaledVector fac1 fac2)) ps
    Curve s ps       -> Curve    s $ map (applyToAbsPoint (scaledVector fac1 fac2)) ps
    a                -> Scale (toFactor fac1) (toFactor fac2) a

  rotated a p
    | modAngle == 0 = p
    | otherwise = case p of
      Scale fac1 fac2 c@(Circle {})
        | modAngle == pi/2 || modAngle == 3*pi/2
                      -> scaled (fromFactor fac2) (fromFactor fac1) c
      Rotate a2 q     -> rotated (a + fromAngle a2) q
      Reflect a2 q    -> reflected (fromAngle a2 + a/2) q
      Translate x y q -> Translate
                          (toPosition $ fromPosition x*cos a - fromPosition y*sin a)
                          (toPosition $ fromPosition x*sin a + fromPosition y*cos a)
                          $ rotated a q
      Color c q       -> Color c $ rotated a q
      Pictures ps     -> Pictures $ map (rotated a) ps
      Polyline s ps   -> Polyline s $ map (applyToAbsPoint (rotatedVector a)) ps
      Curve s ps      -> Curve    s $ map (applyToAbsPoint (rotatedVector a)) ps
      Rectangle s x y
        | fromAngle absAngle >=  pi  -> rotated (modAngle - pi) $ Rectangle s x y
      Circle s r      -> Circle s r
      q               -> Rotate absAngle q
    where
      absAngle = toAngle a
      modAngle = fromAngle absAngle


  reflected a1 (Reflect a2 p)
    | a1 == fromAngle a2 = p
    | otherwise = rotated (a1*2 - fromAngle a2*2) p
  reflected a (Rectangle s x y) = rotated (a*2) $ Rectangle s x y
  reflected _ (Circle s r) = Circle s r
  reflected a (Polyline s ps) = Polyline s $ map (applyToAbsPoint (reflectedPoint a)) ps
  reflected a (Curve s ps) = Curve s $ map (applyToAbsPoint (reflectedPoint a)) ps
  reflected a (Pictures ps) = Pictures $ map (reflected a) ps
  reflected a (Translate x y p) =
    let
      exactX = fromPosition x
      exactY = fromPosition y
      twoTimesSquaredSubOne f = 2 * f a^(2 :: Int) -1
      twoTimesCosSin = 2 * cos a * sin a
    in translated
      (twoTimesSquaredSubOne cos * exactX + twoTimesCosSin * exactY)
      (twoTimesCosSin * exactX + twoTimesSquaredSubOne sin * exactY)
      $ reflected a p
  reflected a (Rotate a2 p) = reflected (a - (fromAngle a2/2)) p
  reflected a (Color c q)   = Color c $ reflected a q
  reflected a p = Reflect (toAngle a) p

  -- TODO: clip free shapes?
  clipped x y = Clip (toSize x) (toSize y)


checkForCircle :: ShapeKind -> Double -> Double -> Double -> NormalizedPicture
checkForCircle _ _ _ 0 = blank
checkForCircle shape a1 a2 r
  | a1 == a2  = blank
  | a1 > a2 = arc a2 a1 r
  | abs (a1 - a2) >= 2*pi = circleKind r
  | otherwise = Arc shape (toAngle a1) (toAngle a2) (toSize r)
  where
    circleKind = case shape of
      Hollow Normal -> circle
      Hollow Thick  -> thickCircle 1
      Solid         -> solidCircle


checkForRectangle :: ShapeKind -> [Point] -> NormalizedPicture
checkForRectangle shape ps = case pointsToRectangle shape ps of
  Nothing -> handlePointList (Polyline shape) ps
  Just r  -> r


handlePointList :: Drawable a => ([AbsPoint] -> a) -> [Point] -> a
handlePointList f ps
    | length noRepeats < 2 = blank
    | otherwise = f $ map toAbsPoint noRepeats
  where
    noRepeats = removeDupes ps


toOpenShape :: [Point] -> [Point]
toOpenShape ps = ps ++ take 1 ps


removeDupes :: Eq a => [a] -> [a]
removeDupes (x:y:xs)
  | x == y    =      rec
  | otherwise =  x : rec
  where rec = removeDupes (y:xs)
removeDupes xs = xs


pointsToRectangle :: ShapeKind -> [Point] -> Maybe NormalizedPicture
pointsToRectangle shapeKind ps
  | isRectangle ps = Just $ translated x y $ rotated angle $ shapeToUse xLen yLen
  | otherwise = Nothing
  where
    (xLen,yLen) = sideLengths ps
    angle = rotationAngle originPs
    (originPs,(x,y)) = atOriginWithOffset (drop 1 ps)
    shapeToUse = case shapeKind of
      Hollow Normal -> rectangle
      Hollow Thick  -> thickRectangle 1
      Solid         -> solidRectangle


toWideRectangle :: ShapeKind -> Double -> Double -> NormalizedPicture
toWideRectangle shape l w
    | l >= w = Rectangle shape (toSize l) $ toSize w
    | otherwise = rotated (pi/2) $ Rectangle shape (toSize w) $ toSize l


shapeKindToRectangle :: ShapeKind -> Double -> Double -> NormalizedPicture
shapeKindToRectangle (Hollow Normal) = rectangle
shapeKindToRectangle (Hollow Thick) = thickRectangle 1
shapeKindToRectangle Solid = solidRectangle


handleFreeShape
  :: Bool
  -> ShapeKind
  -> ShapeKind
  -> [AbsPoint]
  -> [AbsPoint]
  -> NormalizedPicture
handleFreeShape isPolyline s1 s2 ps1 ps2
  | endPs1 == toPoints startPs2
    && s1 == s2
  = func s1 (toPoints $ ps1 ++ restPs2)
  | endPs1 == toPoints startRevPs2
    && s1 == s2
  = func s1 (toPoints $ ps1 ++ endRevPs2)
  | otherwise = Pictures [func s1 (toPoints ps1), func s2 (toPoints ps2)]
    where
      toPoints = map fromAbsPoint
      (startPs2,restPs2) = splitAt 1 ps2
      (startRevPs2, endRevPs2) = splitAt 1 $ reverse ps2
      endPs1 = toPoints $ takeEnd 1 ps1
      solidCurveHelper = handlePointList $ Curve Solid
      solidPolylineHelper = checkForRectangle Solid

      func s = case s of
        (Hollow Normal)
          | isPolyline -> polyline
          | otherwise  -> curve
        (Hollow Thick)
          | isPolyline -> thickPolyline 1
          | otherwise  -> thickCurve 1
        Solid
          | isPolyline -> solidPolylineHelper
          | otherwise  -> solidCurveHelper


{-|
True if the first image contains the second image.
This uses fuzzy comparison.
-}
contains :: NormalizedPicture -> NormalizedPicture -> Bool
p `contains` (Pictures ps) = all (contains p) ps
(Pictures ps) `contains` p = any (`contains` p) ps
p `contains` q = p == q || case p of
  Translate x y pic -> case q of
    Translate x2 y2 innerP -> x2 == x && y == y2 && pic `contains` innerP
    _                      -> pic `contains` q
  Rotate a pic -> case q of
    Rotate a2 innerP -> a2 == a && pic `contains` innerP
    _                -> pic `contains` q
  Reflect a pic -> case q of
    Reflect a2 innerP -> a == a2 && pic `contains` innerP
    _                 -> pic `contains` q
  Scale f1 f2 pic -> case q of
    Scale g1 g2 innerP -> f1 == g1 && f2 == g2 && pic `contains` innerP
    _                  -> pic `contains` q
  Color c pic -> case q of
    Color c2 innerP -> c == c2 && pic `contains` innerP
    _               -> pic `contains` q
  _ -> False


{- |
Returns how often a subpicture appears in the image.
-}
count :: NormalizedPicture -> NormalizedPicture -> Int
count thing inside = minimum $ map singleCount $ getSubPictures thing
  where
    singleCount p = length $ filter (`contains` p) $ getSubPictures inside


stripTranslation :: NormalizedPicture -> NormalizedPicture
stripTranslation (Translate _ _ p) = p
stripTranslation (Color c p) = Color c $ stripTranslation p
stripTranslation p                 = p


{-|
Returns the abstract translation of the image.
Neutral translation if none.
-}
getTranslation :: NormalizedPicture -> (Position, Position)
getTranslation (Translate x y _)   = (x,y)
getTranslation (Color _ p)         = getTranslation p
getTranslation p                   = case p of
  (Polyline _ points) -> absPointsToAbsTranslation points
  (Curve _ points) -> absPointsToAbsTranslation $ drop 1 points
  _ -> (0,0)
  where
    absPointsToAbsTranslation =
      both toPosition . snd . atOriginWithOffset . map fromAbsPoint


{-|
Returns the actual translation of the image.
(0,0) if none.
-}
getExactTranslation :: NormalizedPicture -> (Double, Double)
getExactTranslation = both fromPosition . getTranslation


couldHaveTranslation :: NormalizedPicture -> Bool
couldHaveTranslation Translate {} = True
couldHaveTranslation Polyline {}  = True
couldHaveTranslation Curve {}     = True
couldHaveTranslation (Color _ (Translate {})) = True
couldHaveTranslation _            = False


{-|
Returns the `AbsColor` of the image.
Nothing if it is one of logo or coordinate plane.
Black if none.
-}
getColor :: NormalizedPicture -> Maybe AbsColor
getColor (Color c _) = Just c
getColor Blank       = Nothing
getColor Logo        = Nothing
getColor _           = Just $ Tone 0 0 0


{-|
Returns the abstract scaling factors of the image.
Neutral factors if none.
-}
getScalingFactors :: NormalizedPicture -> (Factor,Factor)
getScalingFactors p = headDef (Same,Same) [(f1,f2) | isBasic p, Scale f1 f2 _ <- universe p]


{-|
Returns actual scaling factors of the image.
(1,1) if none.
-}
getExactScalingFactors :: NormalizedPicture -> (Double,Double)
getExactScalingFactors = both fromFactor . getScalingFactors


{-|
Returns abstract rotation of the image if it has any.
-}
getRotation :: NormalizedPicture -> Maybe Angle
getRotation p = listToMaybe [a | isBasic p, Rotate a _ <- universe p]


{-|
Returns actual rotation of the image if it has any.
-}
getExactRotation :: NormalizedPicture -> Double
getExactRotation = maybe 0 fromAngle . getRotation


{-|
Returns abstract reflection of the image if it has any.
-}
getReflectionAngle :: NormalizedPicture -> Maybe Angle
getReflectionAngle p = listToMaybe [a | isBasic p, Reflect a _ <- universe p]


{-|
Returns actual reflection of the image if it has any.
-}
getExactReflectionAngle :: NormalizedPicture -> Double
getExactReflectionAngle = maybe 0 fromAngle . getReflectionAngle


{-|
Returns abstract radius of the image if it actually a circle or circle segment.
-}
getCircleRadius :: NormalizedPicture -> Maybe Size
getCircleRadius p
  | isBasic p = let elements = universe p
    in listToMaybe $
      [s | Circle _ s  <- elements] ++
      [s | Arc _ _ _ s <- elements]
  | otherwise = Nothing


{-|
Returns actual radius of the image if it actually a circle or circle segment.
-}
getExactCircleRadius :: NormalizedPicture -> Maybe Double
getExactCircleRadius = fmap fromSize . getCircleRadius


{-|
Returns abstract side lengths of the image if it actually a rectangle.
-}
getRectangleLengths :: NormalizedPicture -> Maybe (Size,Size)
getRectangleLengths p = listToMaybe [(sx,sy) | isBasic p, Rectangle _ sx sy <- universe p]


{-|
Returns actual side lengths of the image if it actually a rectangle.
-}
getExactRectangleLengths :: NormalizedPicture -> Maybe (Double,Double)
getExactRectangleLengths = fmap (both fromSize) . getRectangleLengths


{-|
Returns actual list of points in the image if it is a \"free shape\",
[] otherwise.
-}
getExactPointList :: NormalizedPicture -> [Point]
getExactPointList (Curve _ ps) = map fromAbsPoint ps
getExactPointList (Polyline _ ps) = map fromAbsPoint ps
getExactPointList _               = []


-- To access translation before it is abstracted away
getSubPictures :: NormalizedPicture -> [NormalizedPicture]
getSubPictures (Pictures xs) = xs
getSubPictures p = [p]



isBasic :: NormalizedPicture -> Bool
isBasic (Pictures {}) = False
isBasic (Clip {}) = False
isBasic _ = True


{- |
Transform a `NormalizedPicture` into a `Picture`.
Used to compare normalized sample solution to student submission in plain form.
This only makes sense if there's exactly one way to solve the given task.
-}
toConcretePicture :: NormalizedPicture -> P.Picture
toConcretePicture p = P.PRec $ case p of
  Rectangle sk sx sy -> (case sk of
    Hollow Normal -> P.Rectangle
    Hollow Thick  -> P.ThickRectangle 1
    _             -> P.SolidRectangle) (fromSize sx) (fromSize sy)
  Circle sk s -> (case sk of
    Hollow Normal -> P.Circle
    Hollow Thick  -> P.ThickCircle 1
    _             -> P.SolidCircle) (fromSize s)
  Lettering t -> P.Lettering t
  Color c q -> P.Color (fromAbsColor c) $ toConcretePicture q
  Translate x y q -> P.Translate (fromPosition x) (fromPosition y) $ toConcretePicture q
  Scale f1 f2 q -> P.Scale (fromFactor f1) (fromFactor f2) $ toConcretePicture q
  Rotate a q -> P.Rotate (fromAngle a) $ toConcretePicture q
  Pictures qs -> P.Pictures $ map toConcretePicture qs
  CoordinatePlane -> P.CoordinatePlane
  Logo -> P.Logo
  Blank -> P.Blank
  Polyline sk ps -> case sk of
    Hollow Normal -> P.Polyline $ map fromAbsPoint ps
    Hollow Thick  -> P.ThickPolyline 1 $ map fromAbsPoint ps
    _             -> P.SolidPolygon $ init $ map fromAbsPoint ps
  Curve sk ps -> case sk of
    Hollow Normal -> P.Curve $ map fromAbsPoint ps
    Hollow Thick  -> P.ThickCurve 1 $ map fromAbsPoint ps
    _             -> P.SolidClosedCurve $ init $ map fromAbsPoint ps
  Arc sk a1 a2 s -> (case sk of
    Hollow Normal -> P.Arc
    Hollow Thick  -> P.ThickArc 1
    _             -> P.Sector) (fromAngle a1) (fromAngle a2) (fromSize s)
  Reflect a q -> P.Reflect (fromAngle a) $ toConcretePicture q
  Clip sx sy q -> P.Clip (fromSize sx) (fromSize sy) $ toConcretePicture q


validThickness :: Double -> Double
validThickness t
  | t < 0     = error $
      "The line width must be non-negative. " ++
      "(This error was thrown inside the test suite)"
  | otherwise = t
