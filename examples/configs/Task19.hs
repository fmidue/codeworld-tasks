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
# - name-shadowing # in the interest of easy abstracting over 'primes' and 'assortedColors' in g, allow shadowing
- unused-local-binds
- unused-matches
- unused-pattern-binds
# - incomplete-patterns # might reveal list patterns
# - incomplete-uni-patterns # might reveal list patterns

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
- Redundant /=
- Redundant ==
- Redundant bracket
- Redundant if
- Redundant translated
- Use &&
- Use camelCase
- Use elem
- Use even
- Use guards
- Use if
- Use odd
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

configGhcWarnings: []

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
- Use ++
- Use concat
# - Use curry
- Use infix
# - Use isJust
# - Use isNothing
- Use list comprehension
# - Use maybe
- Use negate
- Use notElem
# - Use null
- Use repeat
- Use replicate
- Use splitAt
# - Use uncurry

configLanguageExtensions:
- LambdaCase
- NoTemplateHaskell
- TupleSections
----------
module Task19 where

import CodeWorld
import Prelude hiding (($), (!!))

{- Brace yourself, since this is going to take a bit of
 - explaining. The thing is, we have a new customer, and that customer
 - has some strange wishes. But such is life as a software engineer.
 -
 - So this customer wants a certain test chart design, like this:
 - https://code.world/run.html?mode=haskell&dhash=D8vallegOLFOSRlLLYSvTkg
 -
 - The customer's specification mentions prime numbers. Fortunately,
 - we already have a definition of the infinite list of all prime
 - numbers readily at hand, from a previous project of our company:
 -}

-- To convince yourself that this definition is working, you might 
-- want to copy it into a different file, load that file into ghci, 
-- and run something like 'take 100 primes'. 
primes :: [Integer]
primes = sieve [2..]
  where sieve cs =
          let p = head cs
          in [ p ] ++ sieve [ c | c <- tail cs, c `mod` p /= 0 ]

{- What the customer wants to see on screen is: many 5-by-5 squares
 - with different colors, each square offset from the coordinate
 - system's origin by a prime number in the x-direction. How many such
 - squares there should be, and thus how much of the increasing
 - sequence of prime numbers to use, will be given as a parameter. For
 - example, the specific test chart linked to above uses the first 4
 - prime numbers. Due to the relative spacing of prime numbers,
 - specifically since they will often be closer than 5 to each other,
 - some squares will overlap. In that case, the customer wants the
 - squares created for smaller prime numbers to appear behind the ones
 - for larger prime numbers. That is, working with the first 4 prime
 - numbers again, the image should really be the one linked to above,
 - *not* the following one:
 - https://code.world/run.html?mode=haskell&dhash=Dex05jwjVg9D5ooT7tOybQg
 -
 - Where do we get enough different colors from?
 -
 - Well, fortunately CodeWorld provides exactly what we need, an
 - infinite list 'assortedColors :: [Color]' that we can use.
 -
 - So is that it? Not quite. Since, if we create a large amount of
 - squares this way, the overall image will quickly become larger
 - than the 20x20 coordinate plane of CodeWorld, the customer wants to
 - set an additional parameter for scaling the complete image up or
 - down (mostly down, with factors smaller than 1.0). So the first
 - image linked to above would be the outcome of 'scene 4 1.0', while
 - the following image would be the outcome of 'scene 10 0.3':
 - https://code.world/run.html?mode=haskell&dhash=DokcKG92ZCcopCeGgOxbEBA
 -}

scene :: Int -> Double -> Picture
scene number factor = undefined

main :: IO ()
main = drawingOf (coordinatePlane & scene 10 0.3)

{- Strange wishes by the customer indeed. But hey, the customer is
 - always king. And this customer is paying well.
 -
 - But wait. Unfortunately, there is not only customer-king, but also
 - technical-boss at our company. And this boss of yours is always
 - forward thinking and re-use oriented. "Maybe we will be able to use
 - part of the functionality here for another project later on", the
 - boss said. "So make things modular and as general as possible."
 -
 - Specifically, the instructions were that you should not simply
 - write a monolithic implementation of the 'scene' function above,
 - but actually decompose it into two functions, say 'f' and 'g', to
 - be used as follows for replacing the 'undefined' above:
 -
 -                      f ... (g ...)
 -
 - where 'f' takes some parameter(s) as well as a call to 'g', which
 - itself also takes some parameter(s). And the aim is that 'g' should
 - be *as polymorphic as possible*. So probably you should stuff
 - everything that has to do only with general list manipulation into
 - 'g' and everything that has to do more specifically with pictures
 - and colors etc. into 'f'.
 -
 - It is an essential part of your task to find a reasonable division
 - of work between 'f' and 'g' such that 'g' has a lot of polymorphism
 - (type variables in its type signature).
 -
 - Writing down the type signatures for your functions is mandatory.
 -
 - It would also be good to come up with some meaningful function
 - names for 'f' and 'g'. (The boss was not of much help in that
 - regard.)
 -
 - Also, do not use the !! operator or re-implementations of it.
 -}
----------
module Test (test) where
import qualified Task19
import CodeWorld.Test
import Test.HUnit ((~:), (~?), Test)

import TestHelper (isDeeplyDefined)

test :: [ Test ]
test =
  [ "scene =/= undefined?" ~: isDeeplyDefined (Task19.scene 10 0.3)
  , normalize (Task19.scene 4 1) == correct 4 1 ~?
    "submission can produce first, unscaled sample picture?"
  , normalize (Task19.scene 10 0.3) == correct 10 0.3 ~?
    "submission can produce second, scaled sample picture (scene 10 0.3)?"
  ]

correct :: Int -> Double -> Picture
correct i d = normalize $ dilated d $ pictures
  [ translated (fromIntegral p) 0 $ colored c $ solidRectangle 5 5
  | (c,p) <- reverse $ take i $ zip assortedColors Task19.primes
  ]

