
module CodeWorld (
  Picture,
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

import CodeWorld.Tasks.API
import CodeWorld.Tasks.Reify (Picture)
import CodeWorld.Tasks.Types
import CodeWorld.Tasks.VectorSpace hiding (
  sideLengths,
  rotationAngle,
  isRectangle,
  atOriginWithOffset,
  )


drawingOf :: Picture -> IO ()
drawingOf = const $ pure ()


animationOf :: (Double -> Picture) -> IO ()
animationOf = const $ pure ()


trace :: Text -> a -> a
trace = const id
