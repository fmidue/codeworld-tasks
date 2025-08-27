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
- Redundant bracket
- Use even
# - Use isJust
# - Use isNothing
# - Use null
- Use odd

allowAdding: false
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
- Apply De Morgan law
- Redundant /=
- Redundant ==
- Redundant if
- Use &&
- Use 1
- Use camelCase
# - Use curry
- Use if
- Use infix
# - Use maybe
- Use negate
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
- Redundant translated
- Use translated once

configLanguageExtensions:
- LambdaCase
- NoTemplateHaskell
- TupleSections
----------
module Task07 where

import CodeWorld
import Prelude hiding (($))

-- This time we want to draw an Easter themed image. Create the scene 
-- of an egg hunt: many differently colored eggs of different sizes 
-- are "hidden" inside a garden. 
--
-- You can use the following picture as a reference: 
-- 
--   https://code.world/run.html?mode=haskell&dhash=Dm6BmessB7_uexuGH4ILOVg
--
-- Your picture should obviously contain multiple eggs. You can 
-- include as many eggs as you want, but there should be at least 6. 
-- All of them should have uniquely colored shells and they should 
-- vary in size. They should be spread out above a lawn (green solid
-- rectangle). It's okay if the eggs float above the grass a 
-- bit. They do not have to touch the ground. 
--
-- You can re-use your egg from Task03 if you like. If you do,
-- note that the eggs in the new picture do not contain an egg yolk. 
-- 
-- For this task, you are not allowed to freely add definitions. 
-- You can and should only modify occurrences of 'undefined' below.
-- Adding anything else will result in your submission being rejected. 
-- 
-- Hint: Placing all of the eggs individually would be a lot of
--       unnecessary work. Try to define the scene more abstractly
--       (maybe review the lecture slides for ideas). 

main :: IO ()
main = drawingOf scene

scene :: Picture
scene = undefined
  where
    egg :: Picture
    egg = undefined
----------
module Test (test) where
import qualified Task07
import Data.Maybe (fromJust, isNothing, mapMaybe)
import Data.List.Extra (nubBy, nubOrd)
import Test.HUnit ((~:), (~?), Test(..), assertBool)
import CodeWorld.Test

import qualified TestHarness as TH
import TestHelper (isDeeplyDefined)

test :: [ Test ]
test =
  [ "scene =/= undefined?" ~: isDeeplyDefined Task07.scene
  , onScene (containsElem grass) ~? "There's grass?"
  , onScene (oneOf containsElem eggChoices) ~? "There's at least one egg?"
  , onScene (oneOf (`atLeast` 6) eggChoices) ~? "There are at least 6 eggs?"
  , length (nubBy isSameColor usedColors) >= 6 ~? "Each egg has a unique color?"
  , lengthUniques (map getExactScalingFactors sceneEggs) >= 2 ||
    lengthUniques (map getExactCircleRadius sceneEggs) >= 2 ||
    onScene (oneOf containsElem $ drop 2 eggChoices)
    ~? "There are eggs of different sizes?"
  , onScene (oneOf (\p -> hasRelation (p `isLeftOf` p)) eggChoices <||>
             oneOf (\p -> hasRelation (p `isRightOf` p)) eggChoices
            ) ~? "Eggs are spread out? Maybe you have drawn too many rectangles if they are."
  , onScene (oneOf (\p -> hasRelation (p `isAbove` grass)) eggChoices)
    ~? "Eggs are above the grass?"
  , TestCase $ TH.syntaxCheckWithExts ["LambdaCase","NoTemplateHaskell","TupleSections"] $ \m -> assertBool
      "You are manually placing the eggs. Consider a different approach!"
      $ TH.contains TH.listComprehension $ TH.findTopLevelDeclsOf "scene" m
  , TestCase $ do
      result <- testCSE Task07.scene
      assertBool (fromJust result) (isNothing result)
  ]
  where
    onScene = flip evaluatePred Task07.scene
    grass = colored green someSolidRectangle
    sceneEggs = findAll isEgg $ getComponents Task07.scene
    usedColors = mapMaybe getColor sceneEggs
    eggChoices = [singleEgg, doubleEgg, polyEggSolid, polyEggThick]
    singleEgg = someCircle
    doubleEgg = someSolidCircle & someSolidCircle
    polyEggSolid = someSolidCurve 4
    polyEggThick = someCurve 4
    isEgg p = p `contains` singleEgg || p `contains` doubleEgg ||
              p `contains` polyEggSolid || p `contains` polyEggThick

    lengthUniques :: (Ord a, Eq a) => [a] -> Int
    lengthUniques = length . nubOrd

