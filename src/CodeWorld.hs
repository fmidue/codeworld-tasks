
module CodeWorld (
  -- * Types
  P.Picture,
  T.Color(HSL,RGB,RGBA),

  -- * CodeWorld API
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

  -- * Supplemental Types
  module CodeWorld.Tasks.Types,

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
import CodeWorld.Tasks.Types hiding (Color(..))
import qualified CodeWorld.Tasks.Types as T
import CodeWorld.Tasks.VectorSpace hiding (
  sideLengths,
  rotationAngle,
  isRectangle,
  atOriginWithOffset,
  wasRotatedBy,
  wasTranslatedBy,
  wasScaledBy,
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
