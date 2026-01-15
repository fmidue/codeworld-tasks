{-# language DeriveDataTypeable #-}
{-# language DeriveGeneric #-}
{-# language DeriveAnyClass #-}
{-# language PatternSynonyms #-}
{-# language TypeFamilies #-}
{-# language ViewPatterns #-}

module CodeWorld.Tasks.Picture (
  ReifyPicture,
  Picture(
    Rectangle,
    ThickRectangle,
    SolidRectangle,
    Circle,
    ThickCircle,
    SolidCircle,
    Arc,
    ThickArc,
    Sector,
    Polyline,
    ThickPolyline,
    Polygon,
    ThickPolygon,
    SolidPolygon,
    Curve,
    ThickCurve,
    ClosedCurve,
    ThickClosedCurve,
    SolidClosedCurve,
    Lettering,
    StyledLettering,
    Translate,
    Scale,
    Dilate,
    Color,
    Rotate,
    Reflect,
    Clip,
    And,
    Pictures,
    CoordinatePlane,
    Blank,
    Logo,

    AnyRectangle,
    AnyCircle,
    AnyArc,
    AnyPolyline,
    AnyCurve
  ),
  share,
  toInterface,
  innerPicture,
  hasInnerPicture,
  rectangle,
  thickRectangle,
  solidRectangle,
  circle,
  thickCircle,
  solidCircle,
  arc,
  sector,
  thickArc,
  curve,
  thickCurve,
  closedCurve,
  thickClosedCurve,
  solidClosedCurve,
  polyline,
  thickPolyline,
  polygon,
  thickPolygon,
  solidPolygon,
  lettering,
  styledLettering,
  colored,
  coloured,
  translated,
  scaled,
  dilated,
  rotated,
  reflected,
  clipped,
  pictures,
  (&),
  coordinatePlane,
  codeWorldLogo,
  blank,
  isIn,
  ) where


import Control.DeepSeq                  (NFData)
import Data.Data                        (Data)
import Data.Foldable                    (toList)
import Data.IntMap                      (IntMap, Key)
import Data.List.Extra                  (headDef)
import Data.Reify                       (Graph(..), MuRef(..), reifyGraph)
import Data.Text                        (Text)
import Data.Tuple.Extra                 (both)
import Data.Generics.Uniplate.Data      (children)
import GHC.Generics                     (Generic)
import qualified Data.IntMap            as IM
import qualified Data.Text              as T

import qualified CodeWorld.Tasks.API    as API
import qualified CodeWorld.Tasks.Types  as T
import CodeWorld.Tasks.Color            (Color)
import CodeWorld.Tasks.Types (
  Font,
  ReifyPicture,
  TextStyle,
  Shape(..),
  Style(..)
  )
import CodeWorld.Tasks.VectorSpace (
  Point,
  dilatedPoint,
  vectorDifference,
  rotatedPoint,
  crossProduct,
  dotProduct,
  scaledPoint,
  reflectedPoint,
  vectorLength
  )



{- |
Student facing, basic picture type.
A value of this type can be build using the CodeWorld API.

"CodeWorld.Test" also exports pattern synonyms for all contained constructors.
This allows for easier pattern matching in generic traversals.
-}
newtype Picture = PRec (ReifyPicture Picture)
  {- ^
  The recursive structure of the type is necessary
  for [Reify CSE detection](https://hackage.haskell.org/package/data-reify).
  -}
  deriving (Show,Eq,Ord,Generic,NFData,Data)


pattern Rectangle :: Double -> Double -> Picture
pattern Rectangle x y = PRec (T.Rectangle (Outline Nothing) x y)

pattern ThickRectangle :: Double -> Double -> Double -> Picture
pattern ThickRectangle t x y = PRec (T.Rectangle (Outline (Just t)) x y)

pattern SolidRectangle :: Double -> Double -> Picture
pattern SolidRectangle x y = PRec (T.Rectangle Solid x y)

pattern Circle :: Double -> Picture
pattern Circle r = PRec (T.Circle (Outline Nothing) r)

pattern ThickCircle :: Double -> Double -> Picture
pattern ThickCircle t r = PRec (T.Circle (Outline (Just t)) r)

pattern SolidCircle :: Double -> Picture
pattern SolidCircle r = PRec (T.Circle Solid r)

pattern Polygon :: [Point] -> Picture
pattern Polygon ps = PRec (T.Polyline (Closed (Outline Nothing)) ps)

pattern SolidPolygon :: [Point] -> Picture
pattern SolidPolygon ps = PRec (T.Polyline (Closed Solid) ps)

pattern ThickPolygon :: Double -> [Point] -> Picture
pattern ThickPolygon t ps = PRec (T.Polyline (Closed (Outline (Just t))) ps)

pattern Polyline :: [Point] -> Picture
pattern Polyline ps = PRec (T.Polyline (Open Nothing) ps)

pattern ThickPolyline :: Double -> [Point] -> Picture
pattern ThickPolyline t ps = PRec (T.Polyline (Open (Just t)) ps)

pattern Sector :: Double -> Double -> Double -> Picture
pattern Sector a1 a2 r = PRec (T.Arc Solid a1 a2 r)

pattern Arc :: Double -> Double -> Double -> Picture
pattern Arc a1 a2 r = PRec (T.Arc (Outline Nothing) a1 a2 r)

pattern ThickArc :: Double -> Double -> Double -> Double -> Picture
pattern ThickArc t a1 a2 r = PRec (T.Arc (Outline (Just t)) a1 a2 r)

pattern Curve :: [Point] -> Picture
pattern Curve ps = PRec (T.Curve (Open Nothing) ps)

pattern ThickCurve :: Double -> [Point] -> Picture
pattern ThickCurve t ps = PRec (T.Curve (Open (Just t)) ps)

pattern ClosedCurve :: [Point] -> Picture
pattern ClosedCurve ps = PRec (T.Curve (Closed (Outline Nothing)) ps)

pattern SolidClosedCurve :: [Point] -> Picture
pattern SolidClosedCurve ps = PRec (T.Curve (Closed Solid)ps)

pattern ThickClosedCurve :: Double -> [Point] -> Picture
pattern ThickClosedCurve t ps = PRec (T.Curve (Closed (Outline (Just t))) ps)

pattern Lettering :: Text -> Picture
pattern Lettering t = PRec (T.Lettering t)

pattern StyledLettering :: TextStyle -> Font -> Text -> Picture
pattern StyledLettering ts f t = PRec (T.StyledLettering ts f t)

pattern Color :: Color -> Picture -> Picture
pattern Color c p = PRec (T.Color c p)

pattern Translate :: Double -> Double -> Picture -> Picture
pattern Translate x y p = PRec (T.Translate x y p)

pattern Scale :: Double -> Double -> Picture -> Picture
pattern Scale fac1 fac2 p = PRec (T.Scale fac1 fac2 p)

pattern Dilate :: Double -> Picture -> Picture
pattern Dilate fac p = PRec (T.Dilate fac p)

pattern Rotate :: Double -> Picture -> Picture
pattern Rotate a p = PRec (T.Rotate a p)

pattern Reflect :: Double -> Picture -> Picture
pattern Reflect a p = PRec (T.Reflect a p)

pattern Clip :: Double -> Double -> Picture -> Picture
pattern Clip x y p = PRec (T.Clip x y p)

pattern Pictures :: [Picture] -> Picture
pattern Pictures ps = PRec (T.Pictures ps)

pattern And :: Picture -> Picture -> Picture
pattern And p1 p2 = PRec (T.And p1 p2)

pattern CoordinatePlane :: Picture
pattern CoordinatePlane = PRec T.CoordinatePlane

pattern Logo :: Picture
pattern Logo = PRec T.Logo

pattern Blank :: Picture
pattern Blank = PRec T.Blank

-- Tells the compiler that covering these patterns is exhaustive in a pattern match
{-# COMPLETE
  Rectangle,
  ThickRectangle,
  SolidRectangle,
  Circle,
  ThickCircle,
  SolidCircle,
  Arc,
  ThickArc,
  Sector,
  Polyline,
  ThickPolyline,
  Polygon,
  ThickPolygon,
  SolidPolygon,
  Curve,
  ThickCurve,
  ClosedCurve,
  ThickClosedCurve,
  SolidClosedCurve,
  Lettering,
  StyledLettering,
  Translate,
  Scale,
  Dilate,
  Color,
  Rotate,
  Reflect,
  Clip,
  And,
  Pictures,
  CoordinatePlane,
  Blank,
  Logo
  #-}

-- For internal use
pattern AnyRectangle :: Style -> Double -> Double -> Picture
pattern AnyRectangle s x y = PRec (T.Rectangle s x y)

pattern AnyCircle :: Style -> Double -> Picture
pattern AnyCircle s r = PRec (T.Circle s r)

pattern AnyArc :: Style -> Double -> Double-> Double -> Picture
pattern AnyArc s a1 a2 r = PRec (T.Arc s a1 a2 r)

pattern AnyPolyline :: Shape -> [Point] -> Picture
pattern AnyPolyline s ps = PRec (T.Polyline s ps)

pattern AnyCurve :: Shape -> [Point] -> Picture
pattern AnyCurve s ps = PRec (T.Curve s ps)

{-# COMPLETE
  AnyRectangle,
  AnyCircle,
  AnyArc,
  AnyPolyline,
  AnyCurve,
  Lettering,
  StyledLettering,
  Translate,
  Scale,
  Dilate,
  Color,
  Rotate,
  Reflect,
  Clip,
  And,
  Pictures,
  CoordinatePlane,
  Blank,
  Logo
  #-}


{-|
Draw a hollow, thin rectangle with this length and height.
-}
rectangle :: Double -> Double -> Picture
rectangle = Rectangle

{-|
Draw a hollow rectangle with this line width, length and height.
Specifying a negative line width causes a runtime error (mirrors behaviour in CodeWorld editor).
-}
thickRectangle :: Double -> Double -> Double -> Picture
thickRectangle (validThickness -> t) = ThickRectangle t

{-|
Draw a filled in rectangle with this length and height.
-}
solidRectangle :: Double -> Double -> Picture
solidRectangle = SolidRectangle

{-|
Draw a hollow, thin circle with this radius.
-}
circle :: Double -> Picture
circle = Circle

{-|
Draw a hollow circle with this line width and radius.
Specifying a negative line width or a line width greater than the circles diameter
causes a runtime error (mirrors behaviour in CodeWorld editor).
-}
thickCircle :: Double -> Double -> Picture
thickCircle (validThickness -> t) r
  | t <= 2 * abs r = ThickCircle t r
  | otherwise  = error "The line width of a thickCircle must not be greater than the diameter."

{-|
Draw a filled in circle with this radius.
-}
solidCircle :: Double -> Picture
solidCircle = SolidCircle

{-|
Draw a thin, hollow circle segment with these start and end angles and radius.
-}
arc :: Double -> Double -> Double -> Picture
arc = Arc

{-|
Draw a filled in circle segment with these start and end angles and radius.
This would be @solidArc@ following the usual naming scheme.
-}
sector :: Double -> Double -> Double -> Picture
sector = Sector

{-|
Draw a hollow circle segment with this line width, these start and end angles and radius.
Specifying a negative line width causes a runtime error (mirrors behaviour in CodeWorld editor).
-}
thickArc :: Double -> Double -> Double -> Double -> Picture
thickArc (validThickness -> t) = ThickArc t

{-|
Draw a thin curve passing through the provided points via a number of Bézier splices.
-}
curve :: [Point] -> Picture
curve = Curve

{-|
Draw a curve with this line width passing through the provided points via a number of Bézier splices.
Specifying a negative line width causes a runtime error (mirrors behaviour in CodeWorld editor).
-}
thickCurve :: Double -> [Point] -> Picture
thickCurve (validThickness -> t) = ThickCurve t

{-|
Same as `curve` but adds another splice between the start and end points to close the shape.
-}
closedCurve :: [Point] -> Picture
closedCurve = ClosedCurve

{-|
Same as `thickCurve` but adds another splice between the start and end points to close the shape.
-}
thickClosedCurve :: Double -> [Point] -> Picture
thickClosedCurve (validThickness -> t) = ThickClosedCurve t

{-|
Draw a curve passing through the provided points via a number of Bézier splices.
Adds another splice between the start and end points to close the shape and completely fills the enclosed area.
-}
solidClosedCurve :: [Point] -> Picture
solidClosedCurve = SolidClosedCurve

{-|
Draw a sequence of thin line segments passing through the provided points.
-}
polyline :: [Point] -> Picture
polyline = Polyline

{-|
Draw a sequence of line segments with this line width passing through the provided points.
Specifying a negative line width causes a runtime error (mirrors behaviour in CodeWorld editor).
-}
thickPolyline :: Double -> [Point] -> Picture
thickPolyline (validThickness -> t) = ThickPolyline t

{-|
Same as `polyline` but adds another segment between the start and end points to close the shape.
-}
polygon :: [Point] -> Picture
polygon = Polygon

{-|
Same as `thickPolyline` but adds another segment between the start and end points to close the shape.
-}
thickPolygon :: Double -> [Point] -> Picture
thickPolygon (validThickness -> t) = ThickPolygon t

{-|
Draw a sequence of line segments with this line width passing through the provided points
and completely fill the enclosed area.
-}
solidPolygon :: [Point] -> Picture
solidPolygon = SolidPolygon

{-|
Render this text into an image.
-}
lettering :: Text -> Picture
lettering = Lettering

{-|
Render this text into an image using the provided `TextStyle` and `Font`.
-}
styledLettering :: TextStyle -> Font -> Text -> Picture
styledLettering = StyledLettering

{-|
Apply this `Color` to the image.
-}
colored :: Color -> Picture -> Picture
colored = Color

{-|
Alias for `colored`.
-}
coloured :: Color -> Picture -> Picture
coloured = colored

{-|
Move the image in x and y-direction.
-}
translated :: Double -> Double -> Picture -> Picture
translated = Translate

{-|
Scale the image in x and y-directions using these modifiers.
-}
scaled :: Double -> Double -> Picture -> Picture
scaled= Scale

{-|
Scale the image in both directions using the same modifier.
-}
dilated :: Double -> Picture -> Picture
dilated = Dilate

{-|
Rotate the image around the origin using this angle in radians.
-}
rotated :: Double -> Picture -> Picture
rotated = Rotate

{-|
Reflect the image across a line through the origin with this angle to the x-axis.
-}
reflected :: Double -> Picture -> Picture
reflected = Reflect

{-|
Clip the image in a rectangle with this length and height.
-}
clipped :: Double -> Double -> Picture -> Picture
clipped = Clip

{-|
Compose a list of `Picture`s.
Equivalent to @foldr (&) blank@.
-}
pictures :: [Picture] -> Picture
pictures = Pictures

{-|
Compose two `Picture`s.
The left argument will be drawn on top of the right argument if they overlap.
-}
(&) :: Picture -> Picture -> Picture
a & b = And a b

{-|
A static image of a coordinate plane extending 5 units in all directions.
-}
coordinatePlane :: Picture
coordinatePlane = CoordinatePlane

{-|
A static image of the CodeWorld logo.
-}
codeWorldLogo :: Picture
codeWorldLogo = Logo

{-|
An empty `Picture`.
This is the identity element of `&`.
-}
blank :: Picture
blank = Blank


instance MuRef Picture where
  type DeRef Picture = ReifyPicture
  -- above pattern synonyms are not useable here.
  -- ReifyPicture's type variable needs to be free.
  mapDeRef f (PRec body) = case body of
    T.Color c p             -> T.Color c <$> f p
    T.Translate x y p       -> T.Translate x y <$> f p
    T.Scale x y p           -> T.Scale x y <$> f p
    T.Dilate x p            -> T.Dilate x <$> f p
    T.Rotate a p            -> T.Rotate a <$> f p
    T.Reflect a p           -> T.Reflect a <$> f p
    T.Clip x y p            -> T.Clip x y <$> f p
    T.And a b               -> T.And <$> f a <*> f b
    T.Pictures ps           -> T.Pictures <$> traverse f ps
    p                     -> pure $ case p of
      T.Rectangle s x y       -> T.Rectangle s x y
      T.Circle s r              -> T.Circle s r
      T.Lettering t           -> T.Lettering t
      T.StyledLettering s w t -> T.StyledLettering s w t
      T.Curve s xs              -> T.Curve s xs
      T.Polyline s xs           -> T.Polyline s xs
      T.Arc s a1 a2 r           -> T.Arc s a1 a2 r
      T.CoordinatePlane       -> T.CoordinatePlane
      T.Logo                  -> T.Logo
      T.Blank                 -> T.Blank


share :: Picture -> IO (IntMap (ReifyPicture Int), IntMap (ReifyPicture Int))
share d = do
  Graph nodes s <- reifyGraph d
  let universe = IM.fromList nodes
      refs = IM.insertWith (+) s 1 $ foldr (mapInsertWith . toList . snd) mempty nodes
      multiRefs = IM.intersection universe $ IM.filter (>1) refs
      lut = IM.intersection universe refs
  pure (multiRefs, lut)


mapInsertWith :: [Key] -> IntMap Int -> IntMap Int
mapInsertWith b m = Prelude.foldr (\x acc-> IM.insertWith (+) x (1 :: Int) acc) m b


toInterface :: API.Drawable a => Picture -> a
toInterface p = case p of
  Rectangle x y -> API.rectangle x y
  ThickRectangle t x y -> API.thickRectangle t x y
  SolidRectangle x y -> API.solidRectangle x y
  Circle r -> API.circle r
  ThickCircle t r -> API.thickCircle t r
  SolidCircle r -> API.solidCircle r
  Polygon ps -> API.polygon ps
  SolidPolygon ps -> API.solidPolygon ps
  ThickPolygon t ps -> API.thickPolygon t ps
  ClosedCurve ps -> API.closedCurve ps
  SolidClosedCurve ps -> API.solidClosedCurve ps
  ThickClosedCurve t ps -> API.thickClosedCurve t ps
  Polyline ps -> API.polyline ps
  ThickPolyline t ps -> API.thickPolyline t ps
  Curve ps -> API.curve ps
  ThickCurve t ps -> API.thickCurve t ps
  Sector a1 a2 r -> API.sector a1 a2 r
  Arc a1 a2 r -> API.arc a1 a2 r
  ThickArc t a1 a2 r -> API.thickArc t a1 a2 r
  Lettering t -> API.lettering t
  StyledLettering ts f t -> API.styledLettering ts f t
  Color c q -> API.colored c $ toInterface q
  Translate x y q -> API.translated x y $ toInterface q
  Scale x y q -> API.scaled x y $ toInterface q
  Dilate fac q -> API.dilated fac $ toInterface q
  Rotate a q -> API.rotated a $ toInterface q
  Reflect a q -> API.reflected a $ toInterface q
  Clip x y q -> API.clipped x y $ toInterface q
  Pictures ps -> API.pictures $ map toInterface ps
  And p1 p2 -> toInterface p1 API.& toInterface p2
  CoordinatePlane -> API.coordinatePlane
  Logo -> API.codeWorldLogo
  Blank -> API.blank


{-
The next two functions should never be used on `And` or `Pictures` for this to make any sense.
This is currently the case, but could be problematic if this is used on non-reduced trees
where those constructors could appear somewhere inside the structure.
TODO: Change return type to [Picture] and adjust the tests.
-}

{- |
Returns the contained `Picture` if the argument is not a basic shape and the argument itself if it is.

__Warning: This is intended to be used on non-composite pictures only.__
`Pictures` and `And` will be treated as a basic picture (i.e. the function will behave like `id`)
if used as an argument.
-}
innerPicture :: Picture -> Picture
innerPicture p@(Pictures {}) = p
innerPicture p@(And {})      = p
innerPicture p               = headDef p $ children p

{- |
Returns `True` if the argument is not a basic shape.

__Warning: This is intended to be used on non-composite pictures only.__
`Pictures` and `And` will be treated as a basic picture (i.e. returning `False`) if used as an argument.
-}
hasInnerPicture :: Picture -> Bool
hasInnerPicture (Pictures {}) = False
hasInnerPicture (And {})      = False
hasInnerPicture p             = not $ null $ children p


{- |
Checks if the argument is contained completely inside a box.
The coordinates of the box are given as two points with the following format:

  1. (minimum x-value, maximum x-value)
  2. (minimum y-value, maximum y-value)
-}
isIn :: Picture -> (Point,Point) -> Bool
isIn pic ((lowerX, upperX), (lowerY, upperY)) =
  handle pic ((lowerX,lowerY),(lowerX,upperY),(upperX,upperY),(upperX,lowerY))

handle :: Picture -> (Point,Point,Point,Point) -> Bool
handle p corners@(a,b,c,d) = case p of
  Rectangle x y -> rectangleCheck x y
  ThickRectangle t x y -> rectangleCheck (x+t/2) (y+t/2)
  SolidRectangle x y -> rectangleCheck x y
  Circle r -> circleCheck r
  ThickCircle t r -> circleCheck (r+t/2)
  SolidCircle r -> circleCheck r
  Polygon ps -> pointCheck ps
  SolidPolygon ps -> pointCheck ps
  ThickPolygon t ps -> pointCheck $ thickenLine t ps
  ClosedCurve ps -> pointCheck ps
  SolidClosedCurve ps -> pointCheck ps
  ThickClosedCurve t ps -> pointCheck $ thickenLine t ps
  Polyline ps -> pointCheck ps
  ThickPolyline t ps -> pointCheck $ thickenLine t ps
  Curve ps -> pointCheck ps
  ThickCurve t ps -> pointCheck $ thickenLine t ps
  Sector _ _ r -> circleCheck r
  Arc _ _ r -> circleCheck r
  ThickArc t _ _ r -> circleCheck (r+t/2)
  Lettering t -> textCheck t
  StyledLettering _ _ t ->  textCheck t
  Color _ q -> q `handle` corners
  Translate x y q -> q `handle` mapAll4 (vectorDifference (x,y)) corners
  Scale x y q -> q `handle` mapAll4 (scaledPoint (1/x) (1/y)) corners
  Dilate fac q -> q `handle` mapAll4 (dilatedPoint (1/fac)) corners
  Rotate angle q -> q `handle` mapAll4 (rotatedPoint (-angle)) corners
  Reflect angle q -> q `handle` mapAll4 (reflectedPoint angle) corners
  Clip x y _ -> rectangleCheck x y
  Pictures ps -> all (`handle` corners) ps
  And p1 p2 -> p1 `handle` corners && p2 `handle` corners
  CoordinatePlane -> rectangleCheck 20 20
  Logo -> rectangleCheck 18 7
  Blank -> True
  where
    epsilon = 0.005
    pointCheck = all inRect
    thickenLine t = map (both (+t/2))
    distance v1 v2 =
      let diff = vectorDifference v2 v1
      in  crossProduct diff v1 / vectorLength diff

    circleCheck r = inRect (0,0) && all (\(v1,v2) -> distance v1 v2 + epsilon >= r) [(a,b),(b,c),(c,d),(d,a)]

    rectangleCheck x y = pointCheck [(-(x/2),-(y/2)), (-(x/2),y/2), (x/2,y/2), (x/2,-(y/2))]
    textCheck t = rectangleCheck (fromIntegral $ T.length t) 1

    inRect point =
      let
        ab = vectorDifference b a
        ad = vectorDifference d a
        ap = vectorDifference point a
      in
        -epsilon <= dotProduct ap ab && dotProduct ap ab <= dotProduct ab ab + epsilon &&
        -epsilon <= dotProduct ap ad && dotProduct ap ad <= dotProduct ad ad + epsilon

    mapAll4 f (w,x,y,z) = (f w, f x, f y, f z)


validThickness :: Double -> Double
validThickness t
  | t < 0     = error "The line width must be non-negative."
  | otherwise = t
