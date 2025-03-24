
module CodeWorld.Tasks (
  module CodeWorld.Tasks.Picture,
  module CodeWorld.Tasks.Types,
  module CodeWorld.Tasks.VectorSpace,
  drawingOf,
  ) where


import CodeWorld.Tasks.Picture hiding (toInterface)
import CodeWorld.Tasks.Types
import CodeWorld.Tasks.VectorSpace hiding (
  sideLengths,
  rotationAngle,
  isRectangle,
  atOriginWithOffset,
  )


drawingOf :: Picture -> IO ()
drawingOf = const $ pure ()
