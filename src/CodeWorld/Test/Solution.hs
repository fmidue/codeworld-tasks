
module CodeWorld.Test.Solution (
  PicPredicate,
  containsElem,
  containsElems,
  containsExactElems,
  evaluatePred,
  evaluatePreds,
  hasRelation,
  (<||>),
  option,
  options,
  ifThen,
  thisOften,
  atLeast,
  atMost,
  inRangeOf,
  findMaybe,
  findAll,
  findAllAnd,
  findMaybeAnd,
  findAllActual,
  findMaybeActual,
  findAllActualAnd,
  findMaybeActualAnd,
  oneOf,
  getComponents,
  ) where

import Control.Monad.Reader
import Data.Maybe (listToMaybe)

import CodeWorld.Tasks.Picture (Picture)
import CodeWorld.Test.Abstract (
  AbstractPicture(..),
  contains,
  count,
  getSubPictures,
  )
import CodeWorld.Test.Relative (
  Components(..),
  SpatialQuery,
  toRelative,
  )
import CodeWorld.Test.Rewrite (normalizeAndAbstract)


{- |
Alias for predicates on `Components`.
-}
type PicPredicate = Components -> Bool


{- |
At least one of many predicates evaluates to True.
-}
options :: [Reader Components Bool] -> Reader Components Bool
options = foldr (<||>) (pure False)


{- |
At least one of two predicates evaluates to True.
-}
(<||>) :: Reader Components Bool -> Reader Components Bool -> Reader Components Bool
(<||>) p q = (||) <$> p <*> q


{- |
Alias for (`<||>`)
-}
option :: Reader Components Bool -> Reader Components Bool -> Reader Components Bool
option = (<||>)


{- |
The predicate is satisfied by at least one of the given options.
Use when there's multiple shape primitives a student could use to solve the task.
-}
oneOf :: (a -> Reader Components Bool) -> [a] ->  Reader Components Bool
oneOf p = fmap or . mapM p


-- Apply a function to the list of sub images and retrieve the result
specElems :: (AbstractPicture -> a) -> (Components -> a)
specElems f (Components (ps,_)) = f ps


{- |
Returns the first picture element satisfying the predicate if it exists. (translation is removed)
-}
findMaybe :: (AbstractPicture -> Bool) -> Reader Components (Maybe AbstractPicture)
findMaybe = fmap listToMaybe . findAll


{- |
Returns all picture elements satisfying the predicate. (translation is removed)
-}
findAll :: (AbstractPicture -> Bool) -> Components -> [AbstractPicture]
findAll f (Components (ps,_)) = filter f $ getSubPictures ps


{- |
Returns all subpictures satisfying the predicate. (includes translation)
-}
findAllActual :: (AbstractPicture -> Bool) -> Picture -> [AbstractPicture]
findAllActual f = filter f . getSubPictures . normalizeAndAbstract


{- |
Returns the first subpicture satisfying the predicate if it exists. (includes translation)
-}
findMaybeActual :: (AbstractPicture -> Bool) -> Picture -> Maybe AbstractPicture
findMaybeActual f = listToMaybe . findAllActual f


{- |
Finds all subpictures satisfying a predicate, then applies a function. (includes translation)
-}
findAllActualAnd :: (AbstractPicture -> Bool) -> (AbstractPicture -> a) -> Picture -> [a]
findAllActualAnd f g = map g . findAllActual f


{- |
Finds the first subpicture satisfying a predicate, then applies a function if it exists. (includes translation)
-}
findMaybeActualAnd :: (AbstractPicture -> Bool) -> (AbstractPicture -> a) -> Picture -> Maybe a
findMaybeActualAnd f g = listToMaybe . findAllActualAnd f g


{- |
Finds all picture elements satisfying a predicate, then applies a function. (translation is removed)
-}
findAllAnd :: (AbstractPicture -> Bool) -> (AbstractPicture -> a) -> Reader Components [a]
findAllAnd f g = map g <$> findAll f


{- |
Finds the first element satisfying a predicate, then applies a function if it exists. (translation is removed)
-}
findMaybeAnd :: (AbstractPicture -> Bool) -> (AbstractPicture -> a) -> Reader Components (Maybe a)
findMaybeAnd f = fmap listToMaybe . findAllAnd f


{- |
True if image contains exactly these subpictures and nothing else.
-}
containsExactElems :: [AbstractPicture] -> Reader Components Bool
containsExactElems ps = asks $ specElems (all (\tp -> Pictures ps `contains` tp) . getSubPictures)

{- |
True if image contains at least this subpicture and optionally something else.
-}
containsElem :: AbstractPicture -> Reader Components Bool
containsElem p = asks $ specElems (`contains` p)

{- |
True if image contains at least these subpictures and optionally something else.
-}
containsElems :: [AbstractPicture] -> Reader Components Bool
containsElems ps = asks $ specElems (\t -> all (\p -> t `contains` p) ps)


{- |
True if image contains this subpicture exactly this many times.
-}
thisOften :: AbstractPicture -> Int -> Reader Components Bool
thisOften p amount = asks $ specElems (\ps -> count p ps == amount)


{- |
True if image contains this subpicture at least this many times.
-}
atLeast :: AbstractPicture -> Int -> Reader Components Bool
atLeast p amount = asks $ specElems (\ps -> count p ps >= amount)


{- |
True if image contains this subpicture at most many times.
-}
atMost :: AbstractPicture -> Int -> Reader Components Bool
atMost p amount = asks $ specElems (\ps -> count p ps <= amount)


{- |
True if amount of times this subpicture is contained in the image lies in the specified range.
-}
inRangeOf :: AbstractPicture -> (Int,Int) -> Reader Components Bool
inRangeOf p (lower,upper) = asks $ specElems (\ps -> let occurs = count p ps in occurs >= lower && occurs <= upper)


{- |
Runs the first predicate p, then the second q if p evaluated to `True`.
Does not run q if p evaluates to `False`.
-}
ifThen :: Reader Components Bool -> Reader Components Bool -> Reader Components Bool
ifThen p q = (not <$> p) <||> q


-- Use a predicate on the list of relative positions
specPosition :: SpatialQuery -> PicPredicate
specPosition f (Components (_,rP)) = f rP


{- |
Evaluates given predicates on a student submission.
-}
evaluatePreds :: [Reader Components Bool] -> Picture -> Bool
evaluatePreds fs pic = all (`evaluatePred` pic) fs


{- |
Evaluates the given predicate on a student submission.
-}
evaluatePred :: Reader Components Bool -> Picture -> Bool
evaluatePred f p = runReader f $ getComponents p


{- |
Transforms student submission into spatial `Components` form.
-}
getComponents :: Picture -> Components
getComponents = toRelative . normalizeAndAbstract


{- |
True if image contains the specified spatial relations.
Used with corresponding functions like `CodeWorld.Test.isNorthOf`, `CodeWorld.Test.isLeftOf`, etc.
-}
hasRelation :: SpatialQuery -> Reader Components Bool
hasRelation = asks . specPosition
