module Task07 where

import CodeWorld
import Prelude hiding (($))

-- This time we want to draw an Easter themed image. Create the scene
-- of an egg hunt: many differently colored eggs of different sizes
-- are "hidden" inside a garden.
--
-- You can use the following picture as a reference:
--
--   https://code.world/run.html?mode=haskell&dhash=Dm6BmessB7_uexuGH4ILOVg
--
-- Your picture should obviously contain multiple eggs. You can
-- include as many eggs as you want, but there should be at least 6.
-- All of them should have uniquely colored shells and they should
-- vary in size. They should be spread out above a lawn (green solid
-- rectangle). It's okay if the eggs float above the grass a
-- bit. They do not have to touch the ground.
--
-- You can re-use your egg from Task03 if you like. If you do,
-- note that the eggs in the new picture do not contain an egg yolk.
--
-- For this task, you are not allowed to freely add definitions.
-- You can and should only modify occurrences of 'undefined' below.
-- Adding anything else will result in your submission being rejected.
--
-- Hint: Placing all of the eggs individually would be a lot of
--       unnecessary work. Try to define the scene more abstractly
--       (maybe review the lecture slides for ideas).

main :: IO ()
main = drawingOf scene

scene :: Picture
scene = undefined
  where
    egg :: Picture
    egg = undefined

