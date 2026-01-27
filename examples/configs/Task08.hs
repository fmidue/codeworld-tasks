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
allowModifying: true # because of allowing to change the moon-Picture
allowRemoving: false

configHlintGroups:
- monomorphic
- teaching
- codeworld

# QuickCheck/HUnit testing follows the template check

configGhcWarnings:
- incomplete-patterns
- incomplete-uni-patterns
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
grass = blank

sunOrMoon :: Double -> Picture
sunOrMoon t = blank

-- Do not change the type signature.
sun :: Picture
sun = colored yellow (solidCircle 1)

-- Do not change the type signature.
moon :: Picture
moon = curve [(0,-1),(-0.9,0),(0,1)] & curve [(0,-1),(-0.4,0),(0,1)]

main :: IO ()
main = animationOf scene
----------
{-# language NoMonomorphismRestriction #-}

module Test (test) where
import qualified Task08

import CodeWorld.Test (
  Picture (Rotate),
  hasInnerPicture,
  normalizeAndAbstract,

  withColor,
  green,
  someSolidRectangle,
  white,

  contains,
  findFirstTranslatedThen,
  getExactRotation,
  getExactTranslation,

  (<^^>),
  containsElem,
  hasRelation,
  isBelow,
  oneOf,

  atTime,
  allAt,
  rawImage,
  rawImagesAt,
  noneAt,
  queryAt,

  complain,
  testAnimation,

  samplesUntil,
  )
import Data.Fixed (mod')
import Data.Generics.Uniplate.Data (para)
import Data.List.Extra (nubOrd)
import Data.Maybe (fromMaybe)
import Data.Tuple.Extra ((&&&))
import Test.HUnit ((~:), (~?), Test(..), assertBool, assertString)

import qualified TestHarness as TH
import TestHelper (isDeeplyDefined)

test :: [ Test ]
test =
  [ "scene =/= undefined?" ~: isDeeplyDefined (Task08.scene 1.0)
  , TestCase $ assertString $ testAnimation Task08.scene $ do
      atTime 1 $ do
        complain "This animation does not contain a solid rectangle." $ containsElem someSolidRectangle
        complain "The rectangle is not green." $ containsElem grass
      -- t is not ignored
      complain "Could not detect any movement. Make sure you are not ignoring parameter 't'."
        $ (>1) . lengthUniques <$> rawImagesAt movementCheck

      -- grass does not move or rotate
      complain
        ( "The grass seems to move or disappear at some point during this animation. " ++
          "It should be stationary and not move at all."
        ) $ (==1) . lengthUniques <$> queryAt (100 : movementCheck) getGrassValues

      -- animation keeps going
      complain
        ( "Movement could not be detected after some time has passed. " ++
          "The animation seems to stop at some point. Make sure it runs forever."
        ) $ (>1) . lengthUniques <$> rawImagesAt (map (+100) movementCheck)

      -- there's no white rectangle
      atTime 0 $ do
        complain
          ( "The scene contains a solid white rectangle. " ++
            "This suggests you are trying to conceal the movement of the sun and/or moon at some point."
          ) $ not <$> containsElem cheat

      -- samples contain either sun or moon
      complain
        ( "Detected animation frames with both a sun and a moon. " ++
          "It seems like your sun and moon are moving at the same time."
        ) $ allAt sunMoonCheck sunMoonDetector

      -- sun and moon don't pass under grass
      complain "Your sun and/or moon is moving under the grass!" $
        noneAt sunMoonCheck $ oneOf
          (\p -> hasRelation (normalizeAndAbstract p `isBelow` grass))
          [Task08.sun, Task08.moon]

      -- moon is upright throughout animation
      complain "Your moon spins while moving. It should stay upright during its movement." $
        allAt sunMoonCheck $ do
          image <- rawImage
          let value = fromMaybe 0 (para moonRotation image) `mod'` (2*pi)
          pure (min value (2*pi - value) < 0.001)

  -- Submission contains any of 'sin', 'cos' and 'rotated'
  , TestCase $ TH.syntaxCheckWithExts ["LambdaCase","NoTemplateHaskell","TupleSections"] $ \m -> assertBool
      ( "The sun and moon are not exhibiting circular movement. " ++
        "Please make sure they are not following a parabola shape or other different motions."
      ) $
      any
        (\name -> TH.contains (TH.callTo name m) $ TH.findTopLevelDeclsOf "scene" m)
        ["sin","cos","rotated"]
  ]
  where
    movementCheck = samplesUntil 0.2 5
    sunMoonCheck = samplesUntil 0.2 50
    grass = withColor green someSolidRectangle
    cheat = withColor white someSolidRectangle
    getGrassValues = findFirstTranslatedThen (`contains` grass)
      $ getExactRotation &&& getExactTranslation
    lengthUniques = length . nubOrd
    pictureHas = containsElem . normalizeAndAbstract
    sunMoonDetector =
      pictureHas Task08.sun <^^>
      pictureHas Task08.moon


moonRotation :: Picture -> [Maybe Double] -> Maybe Double
moonRotation (Rotate a q) res = (+ a) <$> maximum res
moonRotation p res
  | hasInnerPicture p = maximum res
  | p == Task08.moon = Just 0
  | otherwise = Nothing

