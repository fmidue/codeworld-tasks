
module CodeWorld.Test.Animation (
  samplesUntil,
  irregularSamples,
  ) where



{- |
An infinite list of animation sampling points.
The basis is made of irrational square roots, exp(1) and π.
The provided samples should not overlap with
any particular phase a student submitted animation may have.
-}
irregularSamples :: [Double]
irregularSamples = 0 : map (/5) (concat $ iterate (map (+3)) base)
  where base = [sqrt (1/5), sqrt (1/2), sqrt 1.2, sqrt 2, sqrt 3, sqrt 5, exp 1, pi]


{- |
Generates irregular sample frames starting from 0 with given step size and cutoff point.
The step size is only an approximation, since the generated values are based on irrational numbers.
-}
samplesUntil :: Double -> Double -> [Double]
samplesUntil stepSize cutOff = fuzzyNub $ filter (`fuzzyElem` [0, stepSize .. cutOff]) $ takeWhile (<= cutOff+epsilon) irregularSamples
  where
    fuzzyElem _ []     = False
    fuzzyElem x (y:xs) = abs (x-y) < epsilon || fuzzyElem x xs

    fuzzyNub (x:y:xs) = x : fuzzyNub (if abs (x-y)+epsilon < stepSize then xs else y:xs)
    fuzzyNub xs = xs

    epsilon = 0.05
