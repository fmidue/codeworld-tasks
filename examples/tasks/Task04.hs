module Task04 where

import CodeWorld
import Prelude hiding (($))
import Data.Text (pack)

-- Draw a rising, moving, then setting sun, as in the sample animation
-- https://code.world/run.html?mode=haskell&dhash=DYBPZOMntsydDgOvvqQNsPQ
--
-- You need not have the exact same positions of the sun at the
-- beginning and end as in our sample animation. For example, the sun
-- can start completely hidden behind the grass line, and can
-- completely disappear behind the grass line at the end. But after
-- having set (Sonnenuntergang) it should stay where it is or
-- completely wink out of existence, not continue to move under the
-- ground or other strange behavior.
--
-- Do not try to "cheat" by letting the sun continue to move under the
-- ground but hiding it behind a white rectangle or anything similar.
--
-- You can work with elementary trigonometry for this task. If you
-- need to refresh your knowledge of trigonometric functions and their
-- connection to circular motion, you may want to have a look at
-- https://en.wikipedia.org/wiki/Unit_circle as well as
-- https://www.geogebra.org/m/Jgt2n9ah
--
-- Of course, you can also work with CodeWorld's 'rotated' function
-- instead.
--
-- In any case, note that angles in CodeWorld are measured in radians,
-- not in degrees.
--
-- Hint: Note that 'scene' is now a function from Double to Picture as
--       opposed to just a Picture in Task01. This additional
--       parameter, here named t, is the number of seconds elapsed
--       since the animation started. As additional help, the current
--       value for t is displayed by the given template (confirming
--       that the program keeps running).

scene :: Double -> Picture
scene t = undefined


-- Do not change the stuff below here!
sceneWithTime :: Double -> Picture
sceneWithTime t = countTime t & scene t

main :: IO ()
main = animationOf sceneWithTime

countTime :: Double -> Picture
countTime t = dilated 0.5 (translated 15 (-6) (lettering (pack ("t = " ++ truncatedTime t))))

truncatedTime :: Double -> String
truncatedTime t =
  let (n,f) = properFraction t
  in show (n :: Int) ++ take 3 (tail (show f))

