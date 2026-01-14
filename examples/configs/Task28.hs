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
# - Eta reduce
- Redundant /=
- Redundant ==
- Redundant bracket
- Redundant if
- Redundant translated
- Use &&
- Use ++
- Use /=
- Use and
- Use camelCase
- Use concat
- Use elem
- Use even
- Use guards
- Use if
# - Use isJust
# - Use isNothing
# - Use null
- Use odd
- Use or
- Use repeat
- Use replicate
- Use ||
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
- Avoid lambda
- Fuse concatMap/map
- Fuse foldr/map
- Fuse mapMaybe/map
- Hoist not
- Use 1
- Use all
- Use any
- Use catMaybes
- Use concatMap
- Use const
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
- NoTemplateHaskell
- TupleSections
----------
module Task28 where

import CodeWorld
import Prelude hiding (($), (!!), (>>=), (=<<), (<*>), head, tail, last, init, take, drop, splitAt, truncate, round, ceiling, floor, fromInteger)
import Data.Maybe

-- Recall the visualization of game levels from Task12. This time we 
-- want to do essentially the same, but use algebraic data types and 
-- avoid list comprehensions. 

-- A level is now a function from integer pairs to 'Maybe Tile', where
-- 'Nothing' corresponds to no tile being present at that coordinate 
-- position (similarly to how we used 0 before). 
--
-- In what follows, we use:

data Tile = Block | Water | Pearl | Air

-- as well as the following type synonym for levels, to make type 
-- signatures more informative: 

type Level = (Integer, Integer) -> Maybe Tile

-- For the compiler there is now no difference whether you write 
-- 'Level' or '(Integer, Integer) -> Maybe Tile' somewhere, but it
-- lets us express intended concepts more clearly. In particular, the 
-- type of a concrete level can now alternatively be given as 
--
--   level :: (Integer, Integer) -> Maybe Tile
-- 
-- or simply as follows: 

level :: Level
level = undefined

-- Replace 'undefined' above by an actual level in the new, Tile-based 
-- encoding. You can either use one of the levels from earlier tasks,
-- or still surprise us with a completely new level of your own making. 

-- As before, specific pictures for the different tiles are given: 

block, water, pearl, air :: Picture

block = colored (light grey) (solidRectangle 1 1)

water = colored blue (solidRectangle 1 1)

pearl = colored purple (solidCircle 0.3) & air

air = colored (translucent blue) (solidRectangle 1 1)

-- The aTile-function has to be adapted to work on the new data type.

aTile :: Tile -> Picture
aTile = undefined

-- Now in order to draw a level in the coordinate range [-10..10] on
-- both dimensions, we want you to implement the (higher-order) 
-- function 'visualize', under the following constraints: 
-- 
-- Do not use list comprehensions or recursion. Range expressions
-- like [a .. b] are still allowed. 
-- 
-- (Do also not use do-notation in the list monad, if you happen to
-- know that concept.)

visualize :: Level -> Picture
visualize lev = undefined

-- Hint: see https://hackage.haskell.org/package/base-4.17.2.1/docs/Data-Maybe.html 
-- for some functions dealing with 'Maybe' values. 

main :: IO ()
main = drawingOf (visualize level)
----------
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Test (test) where
import qualified Task28
import CodeWorld.Test

import Data.Data (Data)
import Data.List (nub)
import Data.Maybe (isJust, maybe)
import Test.HUnit ((~:), (~?), Test(..), assertBool)

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

import qualified TestHarness as TH
import TestHelper (isDeeplyDefined)

test :: [ Test ]
test =
  [ TestCase $ scanSyntax $ \m -> assertBool
      "Submission contains recursion. This was explicitly forbidden by the task description!"
      $ not $ TH.contains (TH.globallySelfRecursive m) $ TH.findTopLevelDeclsOf "visualize" m
  , "Is visualize fully defined?" ~: isDeeplyDefined (Task28.visualize (const Nothing))
  , "aTile =/= undefined?" ~: isDeeplyDefined (Task28.aTile Task28.Block)
  , "level =/= undefined?" ~: isDeeplyDefined (Task28.level (0,0))
  , map Task28.aTile [Task28.Block, Task28.Water, Task28.Pearl, Task28.Air] ==
    [Task28.block, Task28.water, Task28.pearl, Task28.air] ~?
    "aTile maps Tile values to expected pictures?"
  , TestCase $ scanSyntax $ \m -> assertBool
      "visualize uses aTile function?"
      $ TH.contains (TH.callTo "aTile" m) $ TH.findTopLevelDeclsOf "visualize" m
  , TestCase $ scanSyntax $ \m -> assertBool
      "Submission contains a list comprehension. This was explicitly forbidden by the task description!"
      $ not $ TH.contains TH.listComprehension m
  , TestCase $ scanSyntax $ \m -> assertBool
      "Submission contains do-notation. This was explicitly forbidden by the task description!"
      $ not $ TH.contains TH.doNotation m
  , length translations == numberOfTiles ~? "All and only tiles of the level are drawn to the screen?"
  , length translations == length (nub translations) ~? "Each tile is moved to a unique position?"
  , checkDrawnLevel Task28.level ~?
    "The provided level is drawn by 'visualize' according to its definition?"
  , checkDrawnLevel testLevel ~?
    "A hidden level is drawn by 'visualize' according to its definition?"
  ]
  where
    scanSyntax = TH.syntaxCheckWithExts ["NoTemplateHaskell","TupleSections"]
    translations = map getExactTranslation $ findAllActual (`contains` someSolidRectangle) (Task28.visualize Task28.level)
    coords = [(x,y) | x <- [-10..10], y <- [-10..10]]
    numberOfTiles = length $ filter isJust $ map Task28.level coords

    checkDrawnLevel level =
      normalize ( Task28.visualize level ) ==
      normalize ( pictures
        [translated (fromIntegral x) (fromIntegral y) $ maybe blank Task28.aTile $ level (x,y)
        | (x,y) <- coords])


testLevel :: (Integer, Integer) -> Maybe Task28.Tile
testLevel (0,1) = Just Task28.Water
testLevel (1,0) = Just Task28.Air
testLevel (1,1) = Just Task28.Pearl
testLevel (1,2) = Just Task28.Block
testLevel _     = Nothing

deriving instance Generic Task28.Tile
deriving instance NFData Task28.Tile

