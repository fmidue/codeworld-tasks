module Task08 where

import CodeWorld
import Prelude hiding (($))

-- Produce a never ending animation of the day-and-night cycle, as in:
-- https://code.world/run.html?mode=haskell&dhash=DnjQ1h6dp34fkusV07_t77w
--
-- You have some artistic freedom, but we want a non-full moon that
-- holds steadily upright, as in the above animation, and *not* as in:
-- https://code.world/run.html?mode=haskell&dhash=DC3VbjP5p3C_RFIJc_9i60w
--
-- You may (or may not) also want to prettify the moon itself, by
-- using some other CodeWorld primitives to draw it rather than with
-- our simple calls to 'curve'.
--
-- And maybe during night time you want to show some stars in the sky?
-- As in:
-- https://code.world/run.html?mode=haskell&dhash=DLbdDNwLsQlsV_8yC4Ztd5A
--
-- Do not try to "cheat" by letting sun or moon continue to move under
-- the ground but hiding them behind an artificially placed rectangle
-- or anything similar.

scene :: Double -> Picture
scene t = grass & sunOrMoon t

grass :: Picture
grass = blank

sunOrMoon :: Double -> Picture
sunOrMoon t = blank

-- Do not change the type signature. 
sun :: Picture
sun = colored yellow (solidCircle 1)

-- Do not change the type signature.
moon :: Picture
moon = curve [(0,-1),(-0.9,0),(0,1)] & curve [(0,-1),(-0.4,0),(0,1)]

main :: IO ()
main = animationOf scene

