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
- Use /=
- Use even
# - Use isJust
# - Use isNothing
- Use maximum
- Use minimum
# - Use null
- Use odd

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
- missing-signatures
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
# - Eta reduce
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
module Task04 where

import CodeWorld
import Prelude hiding (($))
import Data.Text (pack)

-- Draw a rising, moving, then setting sun, as in the sample animation 
-- https://code.world/run.html?mode=haskell&dhash=DYBPZOMntsydDgOvvqQNsPQ 
-- 
-- You need not have the exact same positions of the sun at the
-- beginning and end as in our sample animation. For example, the sun 
-- can start completely hidden behind the grass line, and can 
-- completely disappear behind the grass line at the end. But after
-- having set (Sonnenuntergang) it should stay where it is or
-- completely wink out of existence, not continue to move under the 
-- ground or other strange behavior. 
-- 
-- Do not try to "cheat" by letting the sun continue to move under the
-- ground but hiding it behind a white rectangle or anything similar. 
-- 
-- You can work with elementary trigonometry for this task. If you
-- need to refresh your knowledge of trigonometric functions and their
-- connection to circular motion, you may want to have a look at 
-- https://en.wikipedia.org/wiki/Unit_circle as well as 
-- https://www.geogebra.org/m/Jgt2n9ah 
--
-- Of course, you can also work with CodeWorld's 'rotated' function 
-- instead. 
--
-- In any case, note that angles in CodeWorld are measured in radians,
-- not in degrees. 
-- 
-- Hint: Note that 'scene' is now a function from Double to Picture as 
--       opposed to just a Picture in Task01. This additional
--       parameter, here named t, is the number of seconds elapsed 
--       since the animation started. As additional help, the current 
--       value for t is displayed by the given template (confirming
--       that the program keeps running).

scene :: Double -> Picture
scene t = undefined

-- Do not change the stuff below here! 
sceneWithTime :: Double -> Picture
sceneWithTime t = countTime t & scene t

main :: IO ()
main = animationOf sceneWithTime

countTime :: Double -> Picture
countTime t = dilated 0.5 (translated 15 (-6) (lettering (pack ("t = " ++ truncatedTime t))))

truncatedTime :: Double -> String
truncatedTime t =
  let (n,f) = properFraction t
  in show (n :: Int) ++ take 3 (tail (show f))
----------
{-# language NoMonomorphismRestriction #-}

module Test (test) where
import qualified Task04
import Data.Maybe (isNothing)
import Data.List.Extra (nubOrd)
import Data.Tuple.Extra ((&&&))
import Test.HUnit ((~:), Test(..), assertBool, assertString)
import CodeWorld.Test (
  withColor,
  green,
  someSolidCircle,
  someSolidRectangle,
  white,
  yellow,

  contains,
  findMaybeActualAnd,
  getExactTranslation,
  getRotation,

  containsElem,
  hasRelation,
  isBelow,

  atTime,
  noneAt,
  rawImagesAt,
  queryAt,

  complain,
  testAnimation,
  testPicture,

  samplesUntil,
  )
import Control.Monad.Reader
import qualified TestHarness as TH
import TestHelper (isDeeplyDefined)

test :: [ Test ]
test =
  [ "scene =/= undefined?" ~: isDeeplyDefined (Task04.scene 1.0)
  , TestCase $ assertString $ testAnimation Task04.scene $ do
      atTime 0 $ do
        complain "This animation does not contain a solid circle."
          $ containsElem someSolidCircle
        complain "This animation does not contain a solid rectangle."
          $ containsElem someSolidRectangle
        complain "The circle is not yellow." $ containsElem sun
        complain "The rectangle is not green." $ containsElem grass
        complain
          ( "The scene contains a solid white rectangle. " ++
            "This suggests you are trying to conceal the movement of the sun at some point."
          )
          $ not <$> containsElem cheat

      complain
        ( "The grass seems to move or disappear at some point during this animation. " ++
          "It should be stationary and the sun should move instead."
        )
        $ (==1) . uniques <$> queryAt (100 : frames) getGrassValues

      complain
        ( "Your animation has changing frames after running for 100 seconds. " ++
          "This suggests your sun is permanently moving instead of setting at some point."
        )
        $ (==1) . uniques <$> rawImagesAt (map (+100) frames)

      complain
        ( "Cannot detect (reasonable) movement in this animation. Make sure parameter 't' is not ignored. " ++
          "Your sun might be moving in a strange way if 't' is actually used for movement."
        )
        $ (>1) . uniques <$> rawImagesAt frames

      complain "Your sun is moving under the grass!"
        $ noneAt (samplesUntil 0.5 30) $ hasRelation $ sun `isBelow` grass

  , TestCase $ TH.syntaxCheckWithExts ["LambdaCase","NoTemplateHaskell","TupleSections"] $ \m -> assertBool
      ( "The sun is not exhibiting circular movement. " ++
        "Please make sure it is not following a parabola shape or other different motions."
      ) $
      any (\name -> TH.contains (TH.ident name) m) ["sin","cos","rotated"]
  ]
  where
    frames = samplesUntil 0.2 5
    grass = withColor green someSolidRectangle
    cheat = withColor white someSolidRectangle
    uniques = length . nubOrd
    getGrassValues = findMaybeActualAnd (`contains` grass)
      $ getRotation &&& getExactTranslation
    sun = withColor yellow someSolidCircle
