
module CodeWorld.Tasks (
  module CodeWorld.Tasks.API,
  Picture,
  drawingOf,
  ) where


import CodeWorld.Tasks.API
import CodeWorld.Tasks.Picture


drawingOf :: Picture -> IO ()
drawingOf = const $ pure ()
