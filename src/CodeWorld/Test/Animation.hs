
module CodeWorld.Test.Animation (
  samplesUntil,
  irregularSamples,
  ) where



irregularSamples :: [Double]
irregularSamples = 0 : map (/5) (concat $ iterate (map (+3)) base)
  where base = [sqrt (1/5), sqrt (1/2), sqrt 1.2, sqrt 2, sqrt 3, sqrt 5, exp 1, pi]


-- generate irregular sample frames with given step size until cutoff point.
samplesUntil :: Double -> Double -> [Double]
samplesUntil stepSize cutOff = fuzzyNub $ filter (`fuzzyElem` [0, stepSize .. cutOff]) $ takeWhile (<= cutOff+epsilon) irregularSamples
  where
    fuzzyElem _ []     = False
    fuzzyElem x (y:xs) = abs (x-y) < epsilon || fuzzyElem x xs

    fuzzyNub (x:y:xs) = x : fuzzyNub (if abs (x-y)+epsilon < stepSize then xs else y:xs)
    fuzzyNub xs = xs

    epsilon = 0.05
