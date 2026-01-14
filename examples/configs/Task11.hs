# the seed used was: 22452873

addCodeWorldButton: true

configGhcErrors:
- deprecation
- empty-enumerations
- identities
- overflowed-literals
- overlapping-patterns
- tabs
- missing-signatures
- name-shadowing
- unused-matches
- unused-pattern-binds
- unused-local-binds

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
allowModifying: false
allowRemoving: false

configHlintGroups:
- monomorphic
- teaching
- codeworld

# QuickCheck/HUnit testing follows the template check

configGhcWarnings:
- incomplete-patterns
- incomplete-uni-patterns

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
----------
module Test (test) where
import qualified Task11

import Data.Generics.Uniplate.Data (para, universe)
import Data.List.Extra (notNull, nubBy, nub)
import Data.Maybe (fromMaybe, mapMaybe)
import CodeWorld.Test (
  Picture(Rotate, Translate),
  normalizeAndAbstract,

  (.&.),
  withColor,
  green,
  someCircle,
  someCurve,
  someSolidCircle,
  someSolidCurve,
  someSolidRectangle,

  contains,
  findAll,
  findAllActual,
  findMaybeActual,
  getColor,
  getComponents,
  getExactCircleRadius,
  getExactRotation,
  getExactScalingFactors,
  getExactTranslation,
  isSameColor,

  (<||>),
  atSamePosition,
  atLeast,
  containsElem,
  evaluatePred,
  hasRelation,
  isAbove,
  isLeftOf,
  isRightOf,
  oneOf,

  samplesUntil,
  )
import Language.Haskell.Exts (Exp(..), SrcSpanInfo)
import Test.HUnit ((~:), (~?), Test(..), assertBool)

import qualified TestHarness as TH
import TestHelper (isDeeplyDefined)

test :: [ Test ]
test =
  [ "scene =/= undefined?" ~: isDeeplyDefined (Task11.scene 1.0)
  , atStart (containsElem grass) ~? "There's grass?"
  , atStart (oneOf containsElem eggChoices) ~? "There's at least one egg?"
  , atStart (oneOf (`atLeast` 6) eggChoices) ~? "There are at least 6 eggs?"
  , length (nubBy isSameColor usedColors) >= 6 ~? "Each egg has a unique color?"
  , lengthUniques (map getExactScalingFactors sceneEggs) >= 2 ||
    lengthUniques (map getExactCircleRadius sceneEggs) >= 2 ||
    atStart (oneOf containsElem curveEggs)  ~? "There are eggs of different sizes?"
  , atStart (oneOf (\p -> hasRelation (p `isLeftOf` p)) eggChoices <||>
             oneOf (\p -> hasRelation (p `isRightOf` p)) eggChoices
            ) ~? "Eggs are spread out? Maybe you have drawn too many rectangles if they are."

  , atStart (oneOf (\p -> hasRelation (p `isAbove` grass)) (take 3 eggChoices) <||>
             hasRelation (polyEggThick `atSamePosition` grass)
            ) ~? "Eggs are above the grass?"

  , lengthUniques (map Task11.scene movementCheck) > 1 ~?
    "Cannot detect movement. Make sure you are not ignoring parameter 't'."

  -- grass is not moving
  , lengthUniques (grassRotations $ 100 : movementCheck) == 1 &&
    lengthUniques (grassMovement $ 100 : movementCheck)  == 1 ~?
    "The grass seems to move or disappear at some point during this animation. " ++
    "It should be stationary and not move at all."

  -- eggs rotation changes
  , lengthUniques (eggRotations widerCheck) >= 6 ||
    atStart (oneOf containsElem curveEggs) ~? "Eggs are swaying?"

  -- animation continues after 100 seconds
  , length (nub (map Task11.scene $ map (+100) movementCheck)) > 1 ~?
    "The animation seems to stop at some point. Make sure it runs indefinitely."

  -- eggs don't rotate more than ~90 degrees
  , all (\a -> a <= pi/2 || a >= 3*pi/2) (eggRotations widerCheck) ~?
    "The eggs seem to sway in an unreasonable fashion. Please make sure they do not clip into the grass completely " ++
    "or 'sway' under the grass."

  -- egg rotation is synchronized
  , all (\t -> lengthUniques (eggRotations [t]) == 1) widerCheck ||
    atStart (oneOf containsElem curveEggs) ~?
    "The eggs do not sway in unison. Make sure their movement is synchronized."

  -- eggs do not rotate around their center
  , correctSwaying (Task11.scene 1) ||
    length (nub (map (gatherTranslations . Task11.scene) movementCheck)) > 1 ~?
    "The swaying motion of your eggs seems wrong. Make sure they do not rotate around their center point. " ++
    "They should be pivoting on the center of their bottom side instead."
  , TestCase $ TH.syntaxCheckWithExts ["LambdaCase","NoTemplateHaskell","TupleSections"] $ assertBool
      "You are manually placing the eggs. Consider a different approach!"
      . TH.contains TH.listComprehension
  ]
  where
    movementCheck = samplesUntil 0.2 3
    widerCheck = samplesUntil 1 50
    sceneAt t = getComponents (Task11.scene t)
    atStart = flip evaluatePred (Task11.scene 0)
    grass = withColor green someSolidRectangle
    sceneEggs = findAll isEgg $ getComponents $ Task11.scene 0
    usedColors = mapMaybe getColor sceneEggs
    eggChoices = [singleEgg, doubleEgg, polyEggSolid, polyEggThick]
    curveEggs = [polyEggSolid, polyEggThick]
    singleEgg = someCircle
    doubleEgg = someSolidCircle .&. someSolidCircle
    polyEggSolid = someSolidCurve 4
    polyEggThick = someCurve 4

    isEgg p = p `contains` singleEgg || p `contains` doubleEgg ||
              p `contains` polyEggSolid || p `contains` polyEggThick

    getElementAt p = findMaybeActual (`contains` p) . Task11.scene
    grassRotations = mapMaybe (fmap getExactRotation . getElementAt grass)
    grassMovement = mapMaybe (fmap getExactTranslation . getElementAt grass)
    eggRotations = concatMap (map getExactRotation . findAllActual isEgg . Task11.scene)

    lengthUniques :: Eq a => [a] -> Int
    lengthUniques = length . nub

    correctSwaying :: Picture -> Bool
    correctSwaying pic = notNull
      [ p | Translate x _ p <- universe pic, x /= 0, hasRotation p]

    hasRotation :: Picture -> Bool
    hasRotation pic = notNull [ p | Rotate _ p <- universe pic, movedUpEgg p]

    movedUpEgg :: Picture -> Bool
    movedUpEgg pic = notNull
      [ p
      | Translate x y p <- universe pic
      , let normalized = normalizeAndAbstract p
      , let eggSize = snd (getExactScalingFactors normalized) *
                      fromMaybe 0 (getExactCircleRadius normalized)
      , x == 0
      , isEgg normalized
      , abs (y - eggSize) <= eggSize*0.5
      ]


gatherTranslations :: Picture -> [(Double,Double)]
gatherTranslations p = [ (x,y) | Translate x y _  <- universe p]

