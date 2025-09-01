
{- |
Module exporting the same interface as provided in the CodeWorld editor.
-}

module CodeWorld (
  -- * CodeWorld API
  -- $API
  P.Picture,
  module Picture,

  -- * Colours
  C.Color(RGB, HSL, RGBA),
  module Color,

  -- * Math Utility
  module VectorSpace,

  -- * Text Rendering Modifiers
  module Types,

  -- * CodeWorld IO Interface
  -- $interface
  drawingOf,
  animationOf,
  trace,
  ) where


import Data.Text                        (Text)

import CodeWorld.Tasks.Picture as Picture (
  rectangle,
  solidRectangle,
  thickRectangle,
  circle,
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
  solidPolygon,
  thickPolygon,
  lettering,
  styledLettering,
  thickCircle,
  translated,
  colored,
  coloured,
  dilated,
  scaled,
  rotated,
  reflected,
  clipped,
  pictures,
  (&),
  coordinatePlane,
  codeWorldLogo,
  blank,
  )
import qualified CodeWorld.Tasks.Picture as P
import CodeWorld.Tasks.Color as Color hiding (Color(..))
import qualified CodeWorld.Tasks.Color as C
import CodeWorld.Tasks.Types as Types
import CodeWorld.Tasks.VectorSpace as VectorSpace (
  Point,
  Vector,
  translatedPoint,
  rotatedPoint,
  reflectedPoint,
  scaledPoint,
  dilatedPoint,
  vectorLength,
  vectorDirection,
  vectorSum,
  vectorDifference,
  scaledVector,
  rotatedVector,
  dotProduct,
  )



{- $API
The CodeWorld t`CodeWorld.Picture` type and corresponding API for composing images.
-}

{- $interface
Entry points for rendering.
Defining the @main@ function in terms of one of these
would draw the provided image to screen.

All of these are dummy functions (no-op IO actions)
as this library does not implement the actual rendering process.
-}

{-|
Render a t`CodeWorld.Picture` onto the canvas.
-}
drawingOf :: P.Picture -> IO ()
drawingOf = const $ pure ()

{-|
Render an animation onto the canvas.
-}
animationOf :: (Double -> P.Picture) -> IO ()
animationOf = const $ pure ()

{-|
Prints a debug message in the console when second argument is evaluated.
-}
trace :: Text -> a -> a
trace = const id
