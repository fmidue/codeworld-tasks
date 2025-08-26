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
scene t = undefined

