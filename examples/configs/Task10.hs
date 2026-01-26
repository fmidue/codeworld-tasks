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
- Use guards
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
# - Use if
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
module Task10 where

import CodeWorld
import Prelude hiding (($))
import Data.Text

-- Some animation functions are given, and you are not allowed to 
-- change them: 

animation1 :: Double -> Picture
animation1 t
  | t < 2     = colored red (circle t)
  | otherwise = blank

animation2 :: Double -> Picture
animation2 t
  | t < 3     = rectangle t (t + 2)
  | otherwise = blank

animation3 :: Double -> Picture
animation3 t
  | t < 2     = solidCircle (t / 2)
  | otherwise = blank

-- The following program overlays these animations. Unfortunately, 
-- that is not exactly what we want. We want the animations to be
-- "played" one after another, and only afterwards do we want the end 
-- notice to be shown. Moreover, we want to scale 'animation2' by a 
-- size factor of 2, and we want 'animation3' to be played at half
-- speed.
-- 
-- The outcome should look as follows: 
-- 
-- https://code.world/run.html?mode=haskell&dhash=DWiaEAgKRHlF5Cg4mKQCv_w
-- 
-- But remember: You are not allowed to change anything in the 
-- definitions of 'animation1', 'animation2' and 'animation3'
-- themselves. You should use them unchanged. And if someone else were
-- to change them, your code in 'scene' should still work with them 
-- (assuming that said someone else has not changed the individual 
-- animations' durations). 
--
-- Of course you *are* allowed to change the conceptual structure and 
-- the content of the 'scene' function. But, as always desirable, do 
-- make sure that your code causes no non-exhaustiveness warnings.
--
-- There is no need for adding new functions, only change 'scene'. 

scene :: Double -> Picture
scene t
  = animation1 t
  & animation2 t
  & animation3 t
  & lettering (pack "The End")

main :: IO ()
main = animationOf scene
----------
module Test (test) where
import qualified Task10

import Data.Text (pack)
import CodeWorld.Test
import Test.HUnit ((~:), Test(..), assertBool, assertString)
import qualified TestHarness as TH
import TestHelper (isDeeplyDefined)

test :: [ Test ]
test =
  [ "scene =/= undefined?" ~: isDeeplyDefined (Task10.scene 1.0)

  -- scene contains all of the predefined animations
  , TestCase $ scanSyntax $ \m -> assertBool
      "At least one of the given animations has not been included in your scene definition."
      $ all (checkForName m) ["animation1","animation2","animation3"]

  -- dilated or scaled was used (necessary for animation2)
  , TestCase $ scanSyntax $ \m -> assertBool
      ( "You are not correctly implementing the required behavior of at least one animation. " ++
        "Consider using 'scaled' or 'dilated' with one of them."
      )
      $ any (checkForName m) ["dilated","scaled"]

  , TestCase $ assertString $ testAnimation Task10.scene $ do
      -- plays animation1 for 2 seconds
      complain "animation1 was not altered and plays on its own for correct amount of time?"
        $ allAtWithTime [0.1,0.2..1.9] $ playsAlone Task10.animation1

      -- plays animation2 for 3 seconds
      complain "animation2 is scaled correctly and plays on its own for correct amount of time?" $
        allAtWithTime [2.1,2.2..4.9] $ (<||>) <$>
          playsAlone (dilated 2 . Task10.animation2 . subtract 2) <*>
          playsAlone (scaled 2 2 . Task10.animation2 . subtract 2)

      -- plays animation3 for 4 seconds
      complain "animation3 plays at half speed on its own for correct amount of time?"
        $ allAtWithTime [5.1,5.2..8.9] $ playsAlone (\t -> Task10.animation3 ((t - 5) / 2))

      -- displays ending message after 9 seconds
      complain "Ending message was not altered and starts at correct timing?"
        $ atTime 9.05 $ (== lettering (pack "The End")) <$> rawImage

      -- ending message is displayed forever
      complain "Ending message persists indefinitely?"
        $ allAtWithTime [10,11,20,50,100,1000] $ playsAlone
          $ const $ lettering $ pack "The End"
  ]
  where
    playsAlone p t = (== p t) <$> rawImage

    checkForName m name = TH.contains (TH.callTo name m) $ TH.findTopLevelDeclsOf "scene" m
    scanSyntax = TH.syntaxCheckWithExts ["LambdaCase","NoTemplateHaskell","TupleSections"]

