module Task09 where

import CodeWorld
import Prelude hiding (($))

-- The given program implements an animation of a circle that grows
-- over time. Modify it so that the circle switches to shrinking after
-- some seconds and after some further seconds stops changing at all,
-- keeping its (positive) size constant from then on.
--
-- The exact sizes the balloon reaches and the rates at which it grows
-- and shrinks are up to you, just try to not let it grow bigger than
-- the screen.
--
-- Additionally, give the circle a different color during each stage
-- (one color while growing, one color while shrinking, one color in
-- the stable state).
--
-- Overall, the result could look as follows:
--
--   https://code.world/run.html?mode=haskell&dhash=DsF4psfXjd1h15OlncXT00w
--
-- Make sure that the animation is smooth as in this example (no jumps).
--
-- And do make sure that your code causes no non-exhaustiveness warnings.

balloon :: Double -> Picture
balloon t = solidCircle (0.5 * t)

main :: IO ()
main = animationOf balloon

