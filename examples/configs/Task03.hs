# the seed used was: 22452873

addCodeWorldButton: true

configGhcErrors:
- deprecation
- empty-enumerations
- identities
- overflowed-literals
- overlapping-patterns
- tabs
- name-shadowing

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
# - Use isJust
# - Use isNothing
# - Use null

allowAdding: true
allowModifying: false
allowRemoving: false

configHlintGroups:
- monomorphic
- teaching
- codeworld

# QuickCheck/HUnit testing follows the template check

configGhcWarnings:
- unused-local-binds
- unused-matches
- unused-pattern-binds

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
- Redundant bracket
- Use camelCase
# - Use curry
- Use infix
# - Use maybe
- Use negate
# - Use uncurry
- Use brighter
- Use darker
- Use dilated
- Use dilatedPoint
- Use duller
- Use lighter
- Use pi
- Use pictures
- Redundant translated
- Use translated once

configLanguageExtensions:
- LambdaCase
- NoTemplateHaskell
- TupleSections
----------
module Task03 where

import CodeWorld
import Prelude hiding (($)) -- just preventing some syntax that might confuse beginners

-- Draw the longitudinal section (the vertical version of a cross 
-- section) of an egg. You may use the following picture as guidance: 
-- 
--   https://code.world/run.html?mode=haskell&dhash=DantMBam23_YjOgmx241lZw
-- 
-- Your picture should consist of an oval gray shell surrounding the 
-- egg white which has a round, yellow yolk inside.
--
-- (Mathematically, the egg form can simply be an ellipse/stretched circle.) 
-- 
-- You can look up how to produce and transform relevant shapes in the 
-- CodeWorld documentation and in the example(s) from the lecture.

main :: IO ()
main = drawingOf scene

scene :: Picture
scene = undefined
----------
module Test (test) where
import qualified Task03
import Test.HUnit ((~:), (~?), Test)
import CodeWorld.Test

import TestHelper (isDeeplyDefined)

test :: [ Test ]
test =
  [ "scene =/= undefined?" ~: isDeeplyDefined Task03.scene
  , onScene (containsElem yolk) ~?
    "The picture does not contain the yellow yolk."
  , onScene (oneOf containsElem [someCircle, solidWhite, polyEgg]) ~?
    "The picture does not contain the egg white."
  , onScene (oneOf containsElem [egg, solidGray, multiEgg, polyEgg]) ~?
    "The shell could not be found. It might have the wrong color (should be grey) " ++
    "or might not be a continuous, round shape."
  , any (uncurry (<) . scalingFactor) [egg,solidGray] ||
    onScene (containsElem polyEgg) ~?
    "The egg shell does not seem to have an oval shape."
  , onScene ( oneOf (\p ->
      hasBroadly (yolk `atSamePosition` p) <||>
      hasBroadly (yolk `isBelow` p)) [egg, multiEgg, polyEgg]
      ) ~?  "The yolk is not inside the egg or has not been positioned correctly inside it."
  ]
  where
    onScene = flip evaluatePred Task03.scene
    scalingFactor p = maybe (1,1) getExactScalingFactors $
      findMaybe (`contains` p) $ getComponents Task03.scene

    egg = colored gray someCircle
    multiEgg = solidGray & solidWhite
    polyEgg = colored gray (someCurve 4)
    yolk = colored yellow someSolidCircle
    solidWhite = colored white someSolidCircle
    solidGray = colored gray someSolidCircle

