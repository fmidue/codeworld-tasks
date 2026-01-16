module Task02 where

import CodeWorld
import Prelude hiding (($)) -- just preventing some syntax that might confuse beginners

-- Draw a picture of a tree similar to:
--
--   https://code.world/run.html?mode=haskell&dhash=DDZ_AqUvWQm3FricOIoNx7A
--
-- Your tree should at least consist of a trunk, two branches and some
-- leaves/crown.
--
-- You can look up how to produce and transform relevant shapes in the
-- CodeWorld documentation and in the example(s) from the lecture.

main :: IO ()
main = drawingOf scene

scene :: Picture
scene =
  translated 0 2 (
    colored brown (
      solidRectangle 0.5 4 &
      translated (-0.75) 2.5 (rotated (pi/4) aBranch) &
      translated 0.75 2.5 (rotated (-pi/4) aBranch)
      ) &
    translated 0 3 (colored green (solidCircle 2))
    )
  where
    aBranch = solidRectangle 0.25 2

-- ==================
-- Alternative Lösung
-- ==================

tree :: Picture
tree =
    colored brown (
      trunk
    & translated 0 2.5 branches)
  & translated 0 3 leaves

trunk :: Picture
trunk = solidRectangle 0.5 4

branch :: Picture
branch = solidRectangle 0.25 2

branches :: Picture
branches =
    translated (-0.75) 0 (rotated (pi/4) branch)
  & translated 0.75 0 (rotated (-pi/4) branch)

leaves :: Picture
leaves = colored green (solidCircle 2)

