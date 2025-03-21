
module CodeWorld.Tasks (
  module CodeWorld.Tasks.API,
  module CodeWorld.Tasks.Normalize,
  Picture,
  drawingOf,
  ) where


import CodeWorld.Tasks.API
import CodeWorld.Tasks.Normalize



type Picture = NormalizedPicture


drawingOf :: Picture -> IO ()
drawingOf = const $ pure ()
