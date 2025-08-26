module Task11 where

import CodeWorld
import Prelude hiding (($), (!!))

-- Let's revisit our egg hunt image, but turn it into an animation
-- now.
--
-- Make all of the Easter eggs gently rock to both sides, in an
-- endless motion. You could think of this as being "swayed by the
-- wind". The eggs may touch or even clip into the lawn during their
-- swaying, so don't be too bothered about that. The resulting
-- animation should look similar to this example:
--
-- https://code.world/run.html?mode=haskell&dhash=DGPtiZDKmhISd5WYhXcHpxA
--
-- This task is of course quite similar to Task07. A solution to
-- that other task based on usage of a list comprehension should be
-- particularly easy to reuse and adapt here.

main :: IO ()
main = animationOf scene

scene :: Double -> Picture
scene t = pictures [ translated x 0.5 (sway d t (dilated d (colored c egg)))
                   | (x,d,c) <- [ (2,0.5,blue)
                                , (4,0.5,orange)
                                , (-0.5,0.75,red)
                                , (-6,0.6,purple)
                                , (5.5,0.6,brown)
                                , (-4,0.75,green) ] ]
          & colored green (solidRectangle 18 1)
  where
    egg :: Picture
    egg = scaled 1 1.25 (thickCircle 0.1 1)

sway :: Double -> Double -> Picture -> Picture
sway d t p = rotated (pi / 4 * sin t) (translated 0 offset p)
  where offset = 1.25 * d

