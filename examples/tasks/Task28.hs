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

