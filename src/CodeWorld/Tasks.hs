
module CodeWorld.Tasks (
  module CodeWorld.Tasks.Picture,
  module CodeWorld.Tasks.Types,
  module CodeWorld.Tasks.VectorSpace,
  drawingOf,
  trace,
  ) where


import Data.Text                        (Text)

import CodeWorld.Tasks.Picture          hiding (toInterface)
import CodeWorld.Tasks.Types
import CodeWorld.Tasks.VectorSpace hiding (
  sideLengths,
  rotationAngle,
  isRectangle,
  atOriginWithOffset,
  )


drawingOf :: Picture -> IO ()
drawingOf = const $ pure ()


trace :: Text -> a -> a
trace = const id