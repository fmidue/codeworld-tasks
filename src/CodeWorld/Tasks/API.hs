
module CodeWorld.Tasks.API (
  Drawable(..),
  ) where


import Data.Text (Text)
import CodeWorld.Tasks.Types (Color, Font, Point, TextStyle)



{- |
The CodeWorld API as a type class.
These are the same functions as seen in "CodeWorld".
This is used to build syntax trees of the type t`CodeWorld.Test.NormalizedPicture`
and another internal type used for CSE detection.

The student facing t`CodeWorld.Test.Picture` type is not a member of this class to improve readability of type errors.
-}
class Drawable a where

  {-|
  Draw a hollow, thin rectangle with this length and height.
  -}
  rectangle :: Double -> Double -> a

  {-|
  Draw a hollow rectangle with this line width, length and height.
  Specifying a negative line width causes a runtime error (mirrors behaviour in CodeWorld editor).
  -}
  thickRectangle :: Double -> Double -> Double -> a

  {-|
  Draw a filled in rectangle with this length and height.
  -}
  solidRectangle :: Double -> Double -> a

  {-|
  Draw a hollow, thin circle with this radius.
  -}
  circle :: Double -> a

  {-|
  Draw a hollow circle with this line width and radius.
  Specifying a negative line width or a line width greater than the circles diameter
  causes a runtime error (mirrors behaviour in CodeWorld editor).
  -}
  thickCircle :: Double -> Double -> a

  {-|
  Draw a filled in circle with this radius.
  -}
  solidCircle :: Double -> a

  {-|
  Draw a thin, hollow circle segment with these start and end angles and radius.
  -}
  arc :: Double -> Double -> Double -> a

  {-|
  Draw a filled in circle segment with these start and end angles and radius.
  This would be `solidArc` following the usual naming scheme.
  -}
  sector :: Double -> Double -> Double -> a

  {-|
  Draw a hollow circle segment with this line width, these start and end angles and radius.
  Specifying a negative line width causes a runtime error (mirrors behaviour in CodeWorld editor).
  -}
  thickArc :: Double -> Double -> Double -> Double -> a

  {-|
  Draw a thin curve passing through the provided points via a number of Bézier splices.
  -}
  curve :: [Point] -> a

  {-|
  Draw a curve with this line width passing through the provided points via a number of Bézier splices.
  Specifying a negative line width causes a runtime error (mirrors behaviour in CodeWorld editor).
  -}
  thickCurve :: Double -> [Point] -> a

  {-|
  Same as `curve` but adds another splice between the start and end points to close the shape.
  -}
  closedCurve :: [Point] -> a

  {-|
  Same as `thickCurve` but adds another splice between the start and end points to close the shape.
  -}
  thickClosedCurve :: Double -> [Point] -> a

  {-|
  Draw a curve passing through the provided points via a number of Bézier splices.
  Adds another splice between the start and end points to close the shape and completely fills the enclosed area.
  -}
  solidClosedCurve :: [Point] -> a

  {-|
  Draw a sequence of thin line segments passing through the provided points.
  -}
  polyline :: [Point] -> a

  {-|
  Draw a sequence of line segments with this line width passing through the provided points.
  -}
  thickPolyline :: Double -> [Point] -> a

  {-|
  Same as `polyline` but adds another segment between the start and end points to close the shape.
  -}
  polygon :: [Point] -> a

  {-|
  Same as `thickPolyline` but adds another segment between the start and end points to close the shape.
  -}
  thickPolygon :: Double -> [Point] -> a

  {-|
  Draw a sequence of line segments with this line width passing through the provided points
  and completely fill the enclosed area.
  -}
  solidPolygon :: [Point] -> a

  {-|
  Render this text into an image.
  -}
  lettering :: Text -> a

  {-|
  Render this text into an image using the provided `TextStyle` and `Font`.
  -}
  styledLettering :: TextStyle -> Font -> Text -> a

  {-|
  Move the image in x and y-direction.
  -}
  translated :: Double -> Double -> a -> a

  {-|
  Apply this `Color` to the image.
  -}
  colored :: Color -> a -> a

  {-|
  Scale the image in both directions using the same modifier.
  -}
  dilated :: Double -> a -> a

  {-|
  Scale the image in x and y-directions using these modifiers.
  -}
  scaled :: Double -> Double -> a -> a

  {-|
  Rotate the image around the origin using this angle in radians.
  -}
  rotated :: Double -> a -> a

  {-|
  Reflect the image across a line through the origin with this angle to the x-axis.
  -}
  reflected :: Double -> a -> a

  {-|
  Clip the image in a rectangle with this length and height.
  -}
  clipped :: Double -> Double -> a -> a

  {-|
  Compose a list of `Picture`s.
  Equivalent to @foldr (&) blank@.
  -}
  pictures :: [a] -> a

  {-|
  Compose two `Pictures`.
  The left argument will be drawn on top of the right argument if they overlap.
  -}
  (&) :: a -> a -> a

  {-|
  A static image of a coordinate plane extending 5 units in all directions.
  -}
  coordinatePlane :: a

  {-|
  A static image of the CodeWorld logo.
  -}
  codeWorldLogo :: a

  {-|
  An empty `Picture`.
  This is the identity element of `&`.
  -}
  blank :: a
