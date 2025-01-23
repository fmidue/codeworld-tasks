
module Examples (
  sampleSolution,
  relativeSampleSolution,
  example1,
  example2,
  example3,
  example4,
  example5,
  threeCircles,

  shareTest1,
  shareTest2,
  shareTest3,
) where


import API                              (Drawable(..))
import Relative                         (RelativePicSpec, northOf)
import Types                            (red, green, yellow)


sampleSolution :: Drawable a => a
sampleSolution = translated 0 6 (colored yellow (solidCircle 1))
               & colored green (solidRectangle 20 2)


relativeSampleSolution :: RelativePicSpec
relativeSampleSolution =
  colored yellow (solidCircle 1) `northOf`
  colored green (solidRectangle 8 10)


example1 :: Drawable a => a
example1 = colored yellow (solidCircle 2)
         & translated 0 (-5) (colored green (solidRectangle 12 3))


example2 :: Drawable a => a
example2 = translated 0 6 (colored yellow(solidCircle 1))
         & colored green (solidRectangle 12 2)


example3 :: Drawable a => a
example3 = grass & sun
  where
    grass = colored green (solidRectangle 20 2)
    sun = translated 0 7 (colored yellow (solidCircle 1.5))


example4 :: Drawable a => a
example4 = ebene & translated 0 9 sonne
  where
    boden = solidRectangle 20 2
    ebene = colored green boden
    sonne = colored yellow (solidCircle 1)


example5 :: Drawable a => a
example5 = colored yellow (solidCircle 1)
         & translated 0 (-3) (colored green (solidRectangle 8 1.5))


threeCircles :: Drawable a => a
threeCircles = colored red (solidCircle 1)
             & translated 2 4 (colored green $ solidCircle 1)
             & translated 0 3 (colored yellow $ solidCircle 1)


shareTest1 :: Drawable a => a
shareTest1 = circle 1 & translated 1 2 (circle 1)


shareTest2 :: Drawable a => a
shareTest2 = let c = circle 1 in c & translated 1 2 c


shareTest3 :: Drawable a => a
shareTest3 = let r = rectangle 2 3 in r & r