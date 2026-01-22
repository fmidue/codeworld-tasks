# the seed used was: 22452873

addCodeWorldButton: true

configGhcErrors:
- deprecation
- empty-enumerations
- identities
- overflowed-literals
- overlapping-patterns
- tabs
- incomplete-patterns
- incomplete-uni-patterns
- missing-signatures
- name-shadowing
- unused-matches
- unused-pattern-binds

configHlintErrors:
- Collapse lambdas
- Evaluate
- Length always non-negative
- Move brackets to avoid $
- Move catMaybes
- Move filter
- Move mapMaybe
- Redundant $
- Redundant flip
- Redundant fromInteger
- Redundant fromIntegral
- Redundant fromMaybe
- Redundant guard
- Redundant id
- Redundant lambda
- Redundant list comprehension
- Redundant maybe
- Redundant multi-way if
- Redundant negate
- Redundant not
- Redundant pair
- Redundant reverse
- Redundant section
- Use + directly
- Use !!
- Use <
- Use <=
- Use ==
- Use >
- Use >=
- Use String
- Use drop
- Use fst
- Use head
- Use id
- Use init
- Use last
- Use left fold instead of right fold
- Use list literal pattern
- Use otherwise
- Use product
- Use right fold instead of left fold
- Use snd
- Use sum
- Use take
- Used otherwise as a pattern
- Using all on tuple
- Using and on tuple
- Using any on tuple
- Using concat on tuple
- Using elem on tuple
- Using foldr on tuple
- Using length on tuple
- Using maximum on tuple
- Using minimum on tuple
- Using null on tuple
- Using or on tuple
- Using product on tuple
- Using sum on tuple
- Redundant bracket
- Redundant translated
- Use /=
- Use camelCase
- Use elem
- Use even
# - Use isJust
# - Use isNothing
- Use maximum
- Use minimum
# - Use null
- Use odd
- Use replicate
- Use translated once

allowAdding: true
allowModifying: true
allowRemoving: false

configHlintGroups:
- monomorphic
- teaching
- codeworld

# QuickCheck/HUnit testing follows the template check

configGhcWarnings:
- unused-local-binds

configHlintRules:
- 'hint: {lhs: drop 1, rhs: tail, note: "Be careful about empty lists, though"}'
- 'warn: {lhs: last (take n x), rhs: x !! (n - 1), note: Check carefully that there is no possibility for index-too-large error}'
- 'warn: {lhs: foldr f c (reverse x), rhs: foldl'' (flip f) c x, note: "reduces laziness", name: Replace a fold by a strict fold}'
- 'warn: {lhs: fold (concat x), rhs: foldMap fold x, name: Redundant concat}'
- 'warn: {lhs: foldMap f (concat x), rhs: foldMap (foldMap f) x, name: Redundant concat}'
- 'warn: {lhs: sin 0, rhs: "0", name: Evaluate}'
- 'warn: {lhs: sin pi, rhs: "0", name: Evaluate}'
- 'warn: {lhs: cos 0, rhs: "1", name: Evaluate}'
- 'warn: {lhs: cos pi, rhs: "-1", name: Evaluate}'
- 'fixity: "infixr 0 &"'
- 'hint: {lhs: "3.14", rhs: pi}'
- 'hint: {lhs: "3.142", rhs: pi}'
- 'hint: {lhs: "3.1416", rhs: pi}'
- 'hint: {lhs: "3.14159", rhs: pi}'
- 'hint: {lhs: "3.141593", rhs: pi}'
- 'hint: {lhs: "3.1415927", rhs: pi}'
- 'hint: {lhs: "3.14159265", rhs: pi}'
- 'hint: {lhs: "6.28", rhs: 2 * pi, name: Use pi}'
- 'hint: {lhs: "6.283", rhs: 2 * pi, name: Use pi}'
- 'hint: {lhs: "6.2832", rhs: 2 * pi, name: Use pi}'
- 'hint: {lhs: "6.28319", rhs: 2 * pi, name: Use pi}'
- 'hint: {lhs: "6.283185", rhs: 2 * pi, name: Use pi}'
- 'hint: {lhs: "6.2831853", rhs: 2 * pi, name: Use pi}'
- 'hint: {lhs: "6.28318531", rhs: 2 * pi, name: Use pi}'
- 'hint: {lhs: "6.283185307", rhs: 2 * pi, name: Use pi}'
- 'hint: {lhs: "1.57", rhs: pi / 2, name: Use pi}'
- 'hint: {lhs: "1.571", rhs: pi / 2, name: Use pi}'
- 'hint: {lhs: "1.5708", rhs: pi / 2, name: Use pi}'
- 'hint: {lhs: "1.57080", rhs: pi / 2, name: Use pi}'
- 'hint: {lhs: "1.570796", rhs: pi / 2, name: Use pi}'
- 'hint: {lhs: "1.5707963", rhs: pi / 2, name: Use pi}'

configHlintSuggestions:
- Avoid lambda using `infix`
- Move guards forward
- Move map inside list comprehension
- Reduce duplication
- Redundant concat
- Redundant take
- Replace a fold by a strict fold
- Too strict if
- Too strict maybe
- Use foldM
- Use max
- Use min
- Use section
- Use tail
- Apply De Morgan law
- Eta reduce
- Redundant /=
- Redundant ==
- Redundant if
- Use &&
- Use ++
- Use 1
# - Use curry
- Use guards
- Use if
- Use infix
- Use list comprehension
# - Use maybe
- Use negate
- Use notElem
- Use repeat
- Use sqrt
# - Use uncurry
- Use ||
- Use brighter
- Use darker
- Use dilated
- Use dilatedPoint
- Use duller
- Use lighter
- Use pi
- Use pictures

configLanguageExtensions:
- LambdaCase
- NoTemplateHaskell
- TupleSections
----------
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
----------
{-# language NoMonomorphismRestriction #-}

module Test (test) where
import qualified Task09

import Data.Maybe (fromJust, mapMaybe)
import Data.List.Extra (nubOrd)
import Control.Monad (join)
import CodeWorld.Test
import Test.HUnit ((~:), Test(..), assertString)
import TestHelper (isDeeplyDefined)
import Control.Monad.Reader (ask)
test :: [ Test ]
test =
  [ "balloon =/= undefined?" ~: isDeeplyDefined (Task09.balloon 1.0)
  , TestCase $ assertString $ testAnimation Task09.balloon $ do
      -- Balloon exists in all samples after t = 0
      complain "Balloon is present at all times?"
        $ allAt framesToCheck $ containsElem balloon

      -- animation stops at some point
      complain "Balloon stops growing or shrinking at some point?"
        $ (==1) . lengthUniques <$> imagesAt (map (+100) $ samplesUntil 0.2 5)

      -- animation includes three different colors
      complain "Balloon has three different colors (each for a few seconds) during animation?"
        $ (==3) . lengthUniques <$> queryAt framesToCheck (getBalloonAnd getColor)

      -- size of the balloon changes
      complain "Balloon starts out growing, then shrinks and finally stops changing at all?"
        $ checkBalloonSizes . mapMaybe join <$> queryAt framesToCheck getBalloonSize

      -- no jumps in animation
      -- (not satisfied with this yet, should be easier to incorporate)
      animation <- ask
      let continuousAt = testContinuous $ fromJust . join . getBalloonSize . animation
      complain "Transition from growing to shrinking is continuous and instant?"
        $ pure $ continuousAt (<= 0)

      complain "Transition from shrinking to stable size is continuous?"
        $ pure $ continuousAt (== 0)
  ]
  where
    framesToCheck = drop 1 $ samplesUntil 0.2 100 -- no balloon at 't = 0'
    balloon = someSolidCircle
    coloredBalloon = someColor balloon
    getBalloonAnd = findMaybeAnd (`contains` balloon)
    -- This doubles as a plain predicate thanks to 'MonadReader r ((->),r)'
    getBalloonSize = getBalloonAnd getExactCircleRadius

    checkBalloonSizes (x:y:xs)
      | x < y = checkBalloonSizes (y:xs)
      | otherwise = shrinks (y:xs)
    shrinks l@(x:y:xs)
      | x > y = shrinks (y:xs)
      | otherwise = lengthUniques l == 1

    lengthUniques = length . nubOrd


testContinuous :: (Double -> Double) -> (Double -> Bool) -> Bool
testContinuous f condition =
    let
      (a,b) = findTransition (0,100) f
    in
      abs (f (b+epsilon) - f (a-epsilon)) <= eta
  where
    delta = 0.001         -- accuracy of inflection point interval
    epsilon = delta / 100 -- step size for slope calculations
    eta = 0.01           -- animation jump tolerance

    findTransition (start,end) f
      | end - start <= delta = (start,end)
      | condition negSlope && condition posSlope = findTransition (start,m) f
      | otherwise = findTransition (m,end) f
      where
        m = (start+end)/2
        negSlope = (f m - f (m-epsilon))/epsilon
        posSlope = (f (m+epsilon) - f m)/epsilon

