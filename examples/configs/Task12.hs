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
# - incomplete-uni-patterns # might reveal list patterns
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
- Eta reduce
- Redundant /=
- Redundant ==
- Redundant bracket
- Redundant if
- Redundant translated
- Use &&
- Use /=
- Use camelCase
- Use elem
- Use even
- Use guards
- Use if
# - Use isJust
# - Use isNothing
- Use maximum
- Use minimum
# - Use null
- Use odd
- Use replicate
- Use ||
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
- Fuse concatMap/map
- Fuse foldr/map
- Fuse mapMaybe/map
- Use ++
- Use 1
- Use catMaybes
- Use concat
- Use concatMap
# - Use curry
- Use find
- Use floor
- Use foldl
- Use foldr
- Use fromMaybe
- Use infix
- Use list comprehension
- Use map once
- Use mapMaybe
# - Use maybe
- Use negate
- Use notElem
- Use repeat
- Use sqrt
- Use tuple-section
# - Use uncurry
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
module Task12 where

import CodeWorld
import Prelude hiding (($), (!!), head, tail, last, init, take, drop, splitAt, truncate, round, ceiling, floor, foldl, foldr, foldMap, fromInteger, mconcat)

-- Suppose we want to implement a jump & run game. First off, we 
-- should care for level design. In a conceptually tiled world, a 
-- level can be thought of as a function assigning to each (x,y) 
-- position of the (assumed infinite) screen some number representing
-- what occupies that tile space. 
-- 
-- For example (note that this takes a pair as input, not two
-- individual coordinate values):

level :: (Integer, Integer) -> Integer
level (x, y)
  | abs x > 6 || abs y > 5             = 0  -- outside of the level
  | abs x == 6 || abs y == 5           = 1  -- for a block
  | y < 1 && x >= y && abs (x - 2) > 2 = 1
  | y < -1                             = 2  -- for water
  | abs y > 2 && abs (x + y - 1) > 5   = 3  -- for a pearl
  | x < -4 && y < 2                    = 3
  | otherwise                          = 4  -- for air

-- We want to produce an actual screen drawing from such a purely 
-- mathematical level description. Fortunately, our graphics designers 
-- have already done some work: 

block, water, pearl, air :: Picture

block = colored (light grey) (solidRectangle 1 1)

water = colored blue (solidRectangle 1 1)

pearl = colored purple (solidCircle 0.3) & air

air = colored (translucent blue) (solidRectangle 1 1)

-- But the remaining work rests with us. The first step is to turn
-- number codes 1, 2, ... used above into the corresponding 
-- pictures. That should not be too hard, right? But make sure that 
-- your implementation of the following function is total, i.e., gives
-- no non-exhaustiveness warning:

aTile :: Integer -> Picture
aTile = undefined

-- Then, we can use that function to produce the overall drawing of 
-- the level. Our visible screen has x-coordinates from -10 to 10, and 
-- likewise for y. And we should really call the 'aTile' function for 
-- all (x,y) combinations in that range, not just some smaller part of
-- the screen, because somebody on the team might still change the 
-- 'level' function to cover more of the visible screen with 
-- interesting stuff, and then we do not want to produce only a part
-- of the drawing.
-- 
-- So we would have to produce 21 * 21 = 441 individual calls to cover 
-- all (x,y) combinations. Surely you can do this more succinctly? 
--
-- If you are thinking of using recursion: don't! We have arts people 
-- on our game development team and don't want to blow their mind. 

scene :: Picture
scene = undefined

-- Extra: Design your own level by providing a different 'level'
--        function of type (Integer, Integer) -> Integer.

main :: IO ()
main = drawingOf scene
----------
module Test (test) where
import qualified Task12
import CodeWorld.Test
import Data.List (nub)
import Test.HUnit ((~:), (~?), Test(..), assertBool, assertString)

import TestHelper (isDefined, isDeeplyDefined)
import qualified TestHarness as TH

test :: [ Test ]
test =
  [ "scene =/= undefined?" ~: isDeeplyDefined Task12.scene
  , "aTile works on Integers?" ~: isDeeplyDefined (Task12.aTile (1 :: Integer))
  , "level is still Integer-typed?" ~: isDefined (Task12.level (0 :: Integer, 0 :: Integer) >= (0 :: Integer))
  , map Task12.aTile [0,1,2,3,4] == [blank, Task12.block, Task12.water, Task12.pearl, Task12.air] ~?
    "aTile maps Integers to expected pictures? (make sure you handle being outside of the level correctly)"
  , TestCase $ TH.syntaxCheckWithExts ["LambdaCase","NoTemplateHaskell","TupleSections"] $ \m -> assertBool
      "scene uses predefined functions?"
      $ all (\name -> TH.contains (TH.ident name) $ TH.findTopLevelDeclsOf "scene" m) ["aTile", "level"]
  , TestCase $ assertString $ testPicture Task12.scene $ do
      complain "Each tile is moved to a unique coordinate?" $ do
        translations <- findAllActualAnd (`contains` someSolidRectangle) getExactTranslation
        pure $ length (nub translations) == length translations
      complain "scene draws the level correctly?" $ do
        image <- normalizedImage
        pure $ image == normalize (pictures
          [translated (fromIntegral x) (fromIntegral y) $ Task12.aTile $ Task12.level (x,y)
          | x <- [-10..10], y <- [-10..10]
          ])
  ]
