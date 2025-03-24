
module CodeWorld.Tasks (
  module CodeWorld.Tasks.Picture,
  drawingOf,
  ) where


import CodeWorld.Tasks.Picture hiding (toInterface)


drawingOf :: Picture -> IO ()
drawingOf = const $ pure ()
