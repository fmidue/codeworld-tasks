
{- |
Module exporting the same interface as provided in the CodeWorld editor.
-}

module CodeWorld (
  -- * CodeWorld API
  P.Picture,
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

  -- * Colours
  module Color,

  -- * Other Types
  module Types,

  -- * CodeWorld Math Functions
  module CodeWorld.Tasks.VectorSpace,

  -- * CodeWorld IO Interface
  drawingOf,
  animationOf,
  trace,
  ) where


import Data.Text                        (Text)

import CodeWorld.Tasks.Picture hiding (Picture)
import qualified CodeWorld.Tasks.Picture as P
import CodeWorld.Tasks.Color as Color
import CodeWorld.Tasks.Types as Types
import CodeWorld.Tasks.VectorSpace (
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
Prints a debug message in the console when argument is evaluated.
-}
trace :: Text -> a -> a
trace = const id
