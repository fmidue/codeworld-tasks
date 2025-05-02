
module CodeWorld (
  P.Picture,
  T.Color(HSL,RGB,RGBA),
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
  module CodeWorld.Tasks.Types,
  module CodeWorld.Tasks.VectorSpace,
  drawingOf,
  animationOf,
  trace,
  ) where


import Data.Text                        (Text)

import CodeWorld.Tasks.Reify hiding (Picture)
import qualified CodeWorld.Tasks.Reify as P
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


drawingOf :: P.Picture -> IO ()
drawingOf = const $ pure ()


animationOf :: (Double -> P.Picture) -> IO ()
animationOf = const $ pure ()


trace :: Text -> a -> a
trace = const id
