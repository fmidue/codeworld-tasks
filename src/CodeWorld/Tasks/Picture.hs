{-# language DeriveTraversable #-}
{-# language DeriveDataTypeable #-}
{-# language DeriveGeneric #-}
{-# language DeriveAnyClass #-}
{-# language TypeFamilies #-}
{-# language ViewPatterns #-}

module CodeWorld.Tasks.Picture (
  ReifyPicture(..),
  Picture(..),
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
import Data.Data                        (Data, Typeable)
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
import CodeWorld.Tasks.Color            (Color)
import CodeWorld.Tasks.Types            (Font, TextStyle)
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


{-|
Basic syntax tree type of the CodeWorld API.
Each API function has a corresponding constructor.

The type variable is necessary for CSE detection via [Reify](https://hackage.haskell.org/package/data-reify).
The method replaces subexpressions with number ids, so we need to be flexible in the wrapped type.
-}
data ReifyPicture a
  = Rectangle Double Double
  | ThickRectangle Double Double Double
  | SolidRectangle Double Double
  | Circle Double
  | ThickCircle Double Double
  | SolidCircle Double
  | Polygon [Point]
  | SolidPolygon [Point]
  | ThickPolygon Double [Point]
  | Polyline [Point]
  | ThickPolyline Double [Point]
  | Sector Double Double Double
  | Arc Double Double Double
  | ThickArc Double Double Double Double
  | Curve [Point]
  | ThickCurve Double [Point]
  | ClosedCurve [Point]
  | SolidClosedCurve [Point]
  | ThickClosedCurve Double [Point]
  | Lettering Text
  | StyledLettering TextStyle Font Text
  | Color Color a
  | Translate Double Double a
  | Scale Double Double a
  | Dilate Double a
  | Rotate Double a
  | Reflect Double a
  | Clip Double Double a
  | Pictures [a]
  | And a a
  | CoordinatePlane
  | Logo
  | Blank
  deriving (Show, Foldable, Eq, Ord, Generic, NFData, Data, Typeable)


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
  deriving (Show,Eq,Ord,Generic,NFData,Data,Typeable)


{-|
Draw a hollow, thin rectangle with this length and height.
-}
rectangle :: Double -> Double -> Picture
rectangle x = PRec . Rectangle x

{-|
Draw a hollow rectangle with this line width, length and height.
Specifying a negative line width causes a runtime error (mirrors behaviour in CodeWorld editor).
-}
thickRectangle :: Double -> Double -> Double -> Picture
thickRectangle (validThickness -> t) x = PRec . ThickRectangle t x

{-|
Draw a filled in rectangle with this length and height.
-}
solidRectangle :: Double -> Double -> Picture
solidRectangle x = PRec . SolidRectangle x

{-|
Draw a hollow, thin circle with this radius.
-}
circle :: Double -> Picture
circle = PRec . Circle

{-|
Draw a hollow circle with this line width and radius.
Specifying a negative line width or a line width greater than the circles diameter
causes a runtime error (mirrors behaviour in CodeWorld editor).
-}
thickCircle :: Double -> Double -> Picture
thickCircle (validThickness -> t) r
  | t <= 2 * r = PRec $ ThickCircle t r
  | otherwise  = error "The line width of a thickCircle must not be greater than the diameter."

{-|
Draw a filled in circle with this radius.
-}
solidCircle :: Double -> Picture
solidCircle = PRec . SolidCircle

{-|
Draw a thin, hollow circle segment with these start and end angles and radius.
-}
arc :: Double -> Double -> Double -> Picture
arc a1 a2 = PRec . Arc a1 a2

{-|
Draw a filled in circle segment with these start and end angles and radius.
This would be @solidArc@ following the usual naming scheme.
-}
sector :: Double -> Double -> Double -> Picture
sector a1 a2 = PRec . Sector a1 a2

{-|
Draw a hollow circle segment with this line width, these start and end angles and radius.
Specifying a negative line width causes a runtime error (mirrors behaviour in CodeWorld editor).
-}
thickArc :: Double -> Double -> Double -> Double -> Picture
thickArc (validThickness -> t) a1 a2 = PRec . ThickArc t a1 a2

{-|
Draw a thin curve passing through the provided points via a number of Bézier splices.
-}
curve :: [Point] -> Picture
curve = PRec . Curve

{-|
Draw a curve with this line width passing through the provided points via a number of Bézier splices.
Specifying a negative line width causes a runtime error (mirrors behaviour in CodeWorld editor).
-}
thickCurve :: Double -> [Point] -> Picture
thickCurve (validThickness -> t) = PRec . ThickCurve t

{-|
Same as `curve` but adds another splice between the start and end points to close the shape.
-}
closedCurve :: [Point] -> Picture
closedCurve = PRec . ClosedCurve

{-|
Same as `thickCurve` but adds another splice between the start and end points to close the shape.
-}
thickClosedCurve :: Double -> [Point] -> Picture
thickClosedCurve (validThickness -> t) = PRec . ThickClosedCurve t

{-|
Draw a curve passing through the provided points via a number of Bézier splices.
Adds another splice between the start and end points to close the shape and completely fills the enclosed area.
-}
solidClosedCurve :: [Point] -> Picture
solidClosedCurve = PRec . SolidClosedCurve

{-|
Draw a sequence of thin line segments passing through the provided points.
-}
polyline :: [Point] -> Picture
polyline = PRec . Polyline

{-|
Draw a sequence of line segments with this line width passing through the provided points.
Specifying a negative line width causes a runtime error (mirrors behaviour in CodeWorld editor).
-}
thickPolyline :: Double -> [Point] -> Picture
thickPolyline (validThickness -> t) = PRec . ThickPolyline t

{-|
Same as `polyline` but adds another segment between the start and end points to close the shape.
-}
polygon :: [Point] -> Picture
polygon = PRec . Polygon

{-|
Same as `thickPolyline` but adds another segment between the start and end points to close the shape.
-}
thickPolygon :: Double -> [Point] -> Picture
thickPolygon (validThickness -> t) = PRec . ThickPolygon t

{-|
Draw a sequence of line segments with this line width passing through the provided points
and completely fill the enclosed area.
-}
solidPolygon :: [Point] -> Picture
solidPolygon = PRec . SolidPolygon

{-|
Render this text into an image.
-}
lettering :: Text -> Picture
lettering = PRec . Lettering

{-|
Render this text into an image using the provided `TextStyle` and `Font`.
-}
styledLettering :: TextStyle -> Font -> Text -> Picture
styledLettering ts f = PRec . StyledLettering ts f

{-|
Apply this `Color` to the image.
-}
colored :: Color -> Picture -> Picture
colored c = PRec . Color c

{-|
Alias for `colored`.
-}
coloured :: Color -> Picture -> Picture
coloured = colored

{-|
Move the image in x and y-direction.
-}
translated :: Double -> Double -> Picture -> Picture
translated x y = PRec . Translate x y

{-|
Scale the image in x and y-directions using these modifiers.
-}
scaled :: Double -> Double -> Picture -> Picture
scaled x y = PRec . Scale x y

{-|
Scale the image in both directions using the same modifier.
-}
dilated :: Double -> Picture -> Picture
dilated a = PRec . Dilate a

{-|
Rotate the image around the origin using this angle in radians.
-}
rotated :: Double -> Picture -> Picture
rotated a = PRec . Rotate a

{-|
Reflect the image across a line through the origin with this angle to the x-axis.
-}
reflected :: Double -> Picture -> Picture
reflected a = PRec . Reflect a

{-|
Clip the image in a rectangle with this length and height.
-}
clipped :: Double -> Double -> Picture -> Picture
clipped x y = PRec . Clip x y

{-|
Compose a list of `Picture`s.
Equivalent to @foldr (&) blank@.
-}
pictures :: [Picture] -> Picture
pictures = PRec . Pictures

{-|
Compose two `Picture`s.
The left argument will drawn on top of the right argument if they overlap.
-}
(&) :: Picture -> Picture -> Picture
a & b = PRec $ And a b

{-|
A static image of a coordinate plane extending 5 units in all directions.
-}
coordinatePlane :: Picture
coordinatePlane = PRec CoordinatePlane

{-|
A static image of the CodeWorld logo.
-}
codeWorldLogo :: Picture
codeWorldLogo = PRec Logo

{-|
An empty `Picture`.
This is the identity element of `&`.
-}
blank :: Picture
blank = PRec Blank


instance MuRef Picture where
  type DeRef Picture = ReifyPicture
  mapDeRef f (PRec body) = case body of
    Color c p             -> Color c <$> f p
    Translate x y p       -> Translate x y <$> f p
    Scale x y p           -> Scale x y <$> f p
    Dilate x p            -> Dilate x <$> f p
    Rotate a p            -> Rotate a <$> f p
    Reflect a p           -> Reflect a <$> f p
    Clip x y p            -> Clip x y <$> f p
    And a b               -> And <$> f a <*> f b
    Pictures ps           -> Pictures <$> traverse f ps
    p                     -> pure $ case p of
      Rectangle x y         -> Rectangle x y
      ThickRectangle t x y  -> ThickRectangle t x y
      SolidRectangle x y    -> SolidRectangle x y
      Circle r              -> Circle r
      ThickCircle t r       -> ThickCircle t r
      SolidCircle r         -> SolidCircle r
      Lettering t           -> Lettering t
      StyledLettering s w t -> StyledLettering s w t
      Curve xs              -> Curve xs
      ThickCurve t xs       -> ThickCurve t xs
      ClosedCurve xs        -> ClosedCurve xs
      SolidClosedCurve xs   -> SolidClosedCurve xs
      ThickClosedCurve xs t -> ThickClosedCurve xs t
      Polygon xs            -> Polygon xs
      SolidPolygon xs       -> SolidPolygon xs
      ThickPolygon xs t     -> ThickPolygon xs t
      Polyline xs           -> Polyline xs
      ThickPolyline xs t    -> ThickPolyline xs t
      Sector a1 a2 r        -> Sector a1 a2 r
      Arc a1 a2 r           -> Arc a1 a2 r
      ThickArc t a1 a2 r    -> ThickArc t a1 a2 r
      CoordinatePlane       -> CoordinatePlane
      Logo                  -> Logo
      Blank                 -> Blank


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
toInterface (PRec p) = case p of
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
innerPicture p@(PRec (Pictures {})) = p
innerPicture p@(PRec (And {}))      = p
innerPicture p                      = headDef p $ children p

{- |
Returns `True` if the argument is not a basic shape.

__Warning: This is intended to be used on non-composite pictures only.__
`Pictures` and `And` will be treated as a basic picture (i.e. returning `False`) if used as an argument.
-}
hasInnerPicture :: Picture -> Bool
hasInnerPicture (PRec (Pictures {})) = False
hasInnerPicture (PRec (And {}))      = False
hasInnerPicture p                    = not $ null $ children p


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
handle (PRec p) corners@(a,b,c,d) = case p of
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
