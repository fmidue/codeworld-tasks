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
grass = colored green (solidRectangle 20 2)

star :: Picture
star = colored yellow (solidPolygon [(-0.5,0),(0.5,0),(0,1)] & solidPolygon [(-0.5,0.65),(0.5,0.65),(0,-0.35)])

stars :: Picture
stars = pictures [ translated (8 * cos (pi/8 * n)) (8 * sin (pi/8 * n)) star | n <- [1..7] ]

sunOrMoon :: Double -> Picture
sunOrMoon t =
  if sin t >= 0
  then translated (6 * cos (pi + t)) (6 * sin t) sun
  else stars & translated (6 * cos t) (6 * sin (pi + t)) moon
--
-- alternatively dealing with the periodicity through recursion:
-- sunOrMoon :: Double -> Picture
-- sunOrMoon t =
--   if t > 2 * pi then sunOrMoon (t - 2 * pi)
--   else if t < pi
--        then translated (6 * cos (pi + t)) (6 * sin t) sun
--        else stars & translated (6 * cos t) (6 * sin (pi + t)) moon
--
-- Note: Case distinction via guards in function definitions has not
-- yet been introduced in the lecture.
--
-- also possible:
-- sunOrMoon t =
--   if even d
--   then rotated (-t') (translated (-6) 0 sun)
--   else stars & rotated (-t') (translated (-6) 0 (rotated t' moon))
--   where d = truncate (t / pi) :: Int
--         t' = t - fromIntegral d * pi
--
-- or, without the stars, and then 'translated x y' being "pulled outwards"
-- sunOrMoon t = translated x y (if even d then sun else moon)
--   where d = truncate (t / pi) :: Int
--         t' = t - fromIntegral d * pi
--         x = - (cos t') * 6
--         y = sin t' * 6

sun :: Picture
sun = colored yellow (solidCircle 1)

moon :: Picture
moon = curve [(0,-1),(-0.9,0),(0,1)] & curve [(0,-1),(-0.4,0),(0,1)]

main :: IO ()
main = animationOf scene

