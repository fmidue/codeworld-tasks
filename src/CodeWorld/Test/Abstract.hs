{-# language DeriveDataTypeable #-}
{-# language FlexibleInstances #-}
{-# language OverloadedStrings #-}
{-# language ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module CodeWorld.Test.Abstract (
  AbstractPicture(..),
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


import Data.Data                        (Data)
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
import qualified CodeWorld.Tasks.Types as PT


{- |
A more abstract syntax tree representing images.
Comparisons between values of this type are intentionally fuzzy:
Concrete number or point values are abstracted into coarser categories.
Notably, those values are not lost and can be retrieved if desired.

The constructors of this type are not exposed.
Values are built using the CodeWorld API.
-}
data AbstractPicture
  = Rectangle !ShapeKind !Size !Size
  | Circle !ShapeKind !Size
  | Lettering !Text
  | Color !AbsColor !AbstractPicture
  | Translate !Position !Position !AbstractPicture
  | Scale !Factor !Factor !AbstractPicture
  | Rotate !Angle !AbstractPicture
  | Pictures [AbstractPicture]
  | CoordinatePlane
  | Logo
  | Blank
  | Polyline !ShapeKind [AbsPoint]
  | Curve !ShapeKind [AbsPoint]
  | Arc !ShapeKind !Angle !Angle !Size
  | Reflect !Angle !AbstractPicture
  | Clip !Size !Size !AbstractPicture
  deriving (Show,Eq,Ord,Data)


instance Drawable AbstractPicture where

  pictures = Pictures

  p1 & p2 = Pictures [p1,p2]

  blank = Blank

  coordinatePlane = CoordinatePlane
  codeWorldLogo = Logo

  circle = Circle (Hollow Normal) . toSize

  solidCircle = Circle Solid . toSize

  thickCircle t = Circle (Hollow $ thickness t) . toSize

  rectangle l w = Rectangle (Hollow Normal) (toSize l) (toSize w)

  solidRectangle l w = Rectangle Solid (toSize l) (toSize w)

  thickRectangle t l w = Rectangle (Hollow $ thickness t) (toSize l) (toSize w)

  arc a1 a2 r = Arc (Hollow Normal) (toAngle a1) (toAngle a2) (toSize r)
  sector a1 a2 r = Arc Solid (toAngle a1) (toAngle a2) (toSize r)
  thickArc t a1 a2 r = Arc (Hollow $ thickness t) (toAngle a1) (toAngle a2) (toSize r)

  curve = Curve (Hollow Normal) . map toAbsPoint
  thickCurve t = Curve (Hollow $ thickness t) . map toAbsPoint
  solidClosedCurve = Curve Solid . map toAbsPoint . toOpenShape

  closedCurve        = curve . toOpenShape
  thickClosedCurve t = thickCurve t . toOpenShape

  polyline        = Polyline (Hollow Normal) . map toAbsPoint
  thickPolyline t = Polyline (Hollow $ thickness t) . map toAbsPoint
  solidPolygon    = Polyline Solid . map toAbsPoint . toOpenShape

  polygon        = polyline . toOpenShape
  thickPolygon t = thickPolyline t . toOpenShape

  lettering = Lettering

  styledLettering _ _ = Lettering

  translated x y = Translate (toPosition x) (toPosition y)

  colored c = Color (toAbsColor c)

  dilated fac = scaled fac fac

  scaled fac1 fac2 = Scale (toFactor fac1) (toFactor fac2)

  rotated a = Rotate (toAngle a)

  reflected a = Reflect (toAngle a)

  -- TODO: clip free shapes?
  clipped x y = Clip (toSize x) (toSize y)


toOpenShape :: [Point] -> [Point]
toOpenShape ps = ps ++ take 1 ps


{-|
True if the first image contains the second image.
This uses fuzzy comparison.
-}
contains :: AbstractPicture -> AbstractPicture -> Bool
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
count :: AbstractPicture -> AbstractPicture -> Int
count thing inside = minimum $ map singleCount $ getSubPictures thing
  where
    singleCount p = length $ filter (`contains` p) $ getSubPictures inside


stripTranslation :: AbstractPicture -> AbstractPicture
stripTranslation (Translate _ _ p) = p
stripTranslation (Color c p) = Color c $ stripTranslation p
stripTranslation p                 = p


{-|
Returns the abstract translation of the image.
Neutral translation if none.
-}
getTranslation :: AbstractPicture -> (Position, Position)
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
getExactTranslation :: AbstractPicture -> (Double, Double)
getExactTranslation = both fromPosition . getTranslation


couldHaveTranslation :: AbstractPicture -> Bool
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
getColor :: AbstractPicture -> Maybe AbsColor
getColor (Color c _) = Just c
getColor Blank       = Nothing
getColor Logo        = Nothing
getColor _           = Just $ Tone 0 0 0


{-|
Returns the abstract scaling factors of the image.
Neutral factors if none.
-}
getScalingFactors :: AbstractPicture -> (Factor,Factor)
getScalingFactors p = headDef (Same,Same) [(f1,f2) | isBasic p, Scale f1 f2 _ <- universe p]


{-|
Returns actual scaling factors of the image.
(1,1) if none.
-}
getExactScalingFactors :: AbstractPicture -> (Double,Double)
getExactScalingFactors = both fromFactor . getScalingFactors


{-|
Returns abstract rotation of the image if it has any.
-}
getRotation :: AbstractPicture -> Maybe Angle
getRotation p = listToMaybe [a | isBasic p, Rotate a _ <- universe p]


{-|
Returns actual rotation of the image if it has any.
-}
getExactRotation :: AbstractPicture -> Double
getExactRotation = maybe 0 fromAngle . getRotation


{-|
Returns abstract reflection of the image if it has any.
-}
getReflectionAngle :: AbstractPicture -> Maybe Angle
getReflectionAngle p = listToMaybe [a | isBasic p, Reflect a _ <- universe p]


{-|
Returns actual reflection of the image if it has any.
-}
getExactReflectionAngle :: AbstractPicture -> Double
getExactReflectionAngle = maybe 0 fromAngle . getReflectionAngle


{-|
Returns abstract radius of the image if it actually a circle or circle segment.
-}
getCircleRadius :: AbstractPicture -> Maybe Size
getCircleRadius p
  | isBasic p = let elements = universe p
    in listToMaybe $
      [s | Circle _ s  <- elements] ++
      [s | Arc _ _ _ s <- elements]
  | otherwise = Nothing


{-|
Returns actual radius of the image if it actually a circle or circle segment.
-}
getExactCircleRadius :: AbstractPicture -> Maybe Double
getExactCircleRadius = fmap fromSize . getCircleRadius


{-|
Returns abstract side lengths of the image if it actually a rectangle.
-}
getRectangleLengths :: AbstractPicture -> Maybe (Size,Size)
getRectangleLengths p = listToMaybe [(sx,sy) | isBasic p, Rectangle _ sx sy <- universe p]


{-|
Returns actual side lengths of the image if it actually a rectangle.
-}
getExactRectangleLengths :: AbstractPicture -> Maybe (Double,Double)
getExactRectangleLengths = fmap (both fromSize) . getRectangleLengths


{-|
Returns actual list of points in the image if it is a \"free shape\",
[] otherwise.
-}
getExactPointList :: AbstractPicture -> [Point]
getExactPointList (Curve _ ps) = map fromAbsPoint ps
getExactPointList (Polyline _ ps) = map fromAbsPoint ps
getExactPointList _               = []


-- To access translation before it is abstracted away
getSubPictures :: AbstractPicture -> [AbstractPicture]
getSubPictures (Pictures xs) = xs
getSubPictures p = [p]



isBasic :: AbstractPicture -> Bool
isBasic (Pictures {}) = False
isBasic (Clip {}) = False
isBasic _ = True


{- |
Transform an `AbstractPicture` into a t`CodeWorld.Test.Picture`.
Used to compare normalized sample solution to student submission in plain form.
This only makes sense if there's exactly one way to solve the given task.
-}
toConcretePicture :: AbstractPicture -> P.Picture
toConcretePicture p = case p of
  Rectangle sk sx sy -> P.AnyRectangle (toStyle sk) (fromSize sx) (fromSize sy)
  Circle sk s -> P.AnyCircle (toStyle sk) (fromSize s)
  Lettering t -> P.Lettering t
  Color c q -> P.Color (fromAbsColor c) $ toConcretePicture q
  Translate x y q -> P.Translate (fromPosition x) (fromPosition y) $ toConcretePicture q
  Scale f1 f2 q -> P.Scale (fromFactor f1) (fromFactor f2) $ toConcretePicture q
  Rotate a q -> P.Rotate (fromAngle a) $ toConcretePicture q
  Pictures qs -> P.Pictures $ map toConcretePicture qs
  CoordinatePlane -> P.CoordinatePlane
  Logo -> P.Logo
  Blank -> P.Blank
  Polyline sk ps -> uncurry P.AnyPolyline $ toShape sk $ map fromAbsPoint ps
  Curve sk ps -> uncurry P.AnyCurve $ toShape sk $ map fromAbsPoint ps
  Arc sk a1 a2 s -> P.AnyArc (toStyle sk) (fromAngle a1) (fromAngle a2) (fromSize s)
  Reflect a q -> P.Reflect (fromAngle a) $ toConcretePicture q
  Clip sx sy q -> P.Clip (fromSize sx) (fromSize sy) $ toConcretePicture q
  where
    toStyle (Hollow Normal) = PT.Outline Nothing
    toStyle (Hollow Thick) = PT.Outline $ Just 1
    toStyle Solid = PT.Solid

    toShape (Hollow Normal) ps = (PT.Open Nothing, ps)
    toShape (Hollow Thick) ps = (PT.Open $ Just 1, ps)
    toShape Solid ps = (PT.Closed PT.Solid, init ps)
