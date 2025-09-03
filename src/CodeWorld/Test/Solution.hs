
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


import Data.Maybe (listToMaybe)

import CodeWorld.Tasks.Picture (Picture, toInterface)
import CodeWorld.Test.Normalize (
  NormalizedPicture(..),
  contains,
  count,
  getSubPictures,
  )
import CodeWorld.Test.Relative (
  Components(..),
  SpatialQuery,
  toRelative,
  )


{- |
Alias for predicates on `Components`.
-}
type PicPredicate = Components -> Bool


{- |
At least one of many predicates evaluates to True.
-}
options :: [PicPredicate] -> PicPredicate
options ps c = any (\p -> p c) ps


{- |
At least one of two predicates evaluates to True.
-}
(<||>) :: PicPredicate -> PicPredicate -> PicPredicate
(<||>) p q c = p c || q c


{- |
Alias for (`<||>`)
-}
option :: PicPredicate -> PicPredicate -> PicPredicate
option = (<||>)


{- |
The predicate is satisfied by at least one of the given options.
Use when there's multiple shape primitives a student could use to solve the task.
-}
oneOf :: (a -> PicPredicate) -> [a] ->  PicPredicate
oneOf p = foldr ((<||>) . p) (const False)


-- Use a predicate on the list of sub images
specElems :: (NormalizedPicture -> Bool) -> PicPredicate
specElems f (Components (ps,_)) = f ps


{- |
Returns the first picture element satisfying the predicate if it exists. (translation is removed)
-}
findMaybe :: (NormalizedPicture -> Bool) -> Components -> Maybe NormalizedPicture
findMaybe f = listToMaybe . findAll f


{- |
Returns all picture elements satisfying the predicate. (translation is removed)
-}
findAll :: (NormalizedPicture -> Bool) -> Components -> [NormalizedPicture]
findAll f (Components (ps,_)) = filter f $ getSubPictures ps


{- |
Returns all subpictures satisfying the predicate. (includes translation)
-}
findAllActual :: (NormalizedPicture -> Bool) -> Picture -> [NormalizedPicture]
findAllActual f = filter f . getSubPictures . toInterface


{- |
Returns the first subpicture satisfying the predicate if it exists. (includes translation)
-}
findMaybeActual :: (NormalizedPicture -> Bool) -> Picture -> Maybe NormalizedPicture
findMaybeActual f = listToMaybe . findAllActual f


{- |
Finds all subpictures satisfying a predicate, then applies a function. (includes translation)
-}
findAllActualAnd :: (NormalizedPicture -> Bool) -> (NormalizedPicture -> a) -> Picture -> [a]
findAllActualAnd f g = map g . findAllActual f


{- |
Finds the first subpicture satisfying a predicate, then applies a function if it exists. (includes translation)
-}
findMaybeActualAnd :: (NormalizedPicture -> Bool) -> (NormalizedPicture -> a) -> Picture -> Maybe a
findMaybeActualAnd f g = listToMaybe . findAllActualAnd f g


{- |
Finds all picture elements satisfying a predicate, then applies a function. (translation is removed)
-}
findAllAnd :: (NormalizedPicture -> Bool) -> (NormalizedPicture -> a) -> Components -> [a]
findAllAnd f g = map g . findAll f


{- |
Finds the first element satisfying a predicate, then applies a function if it exists. (translation is removed)
-}
findMaybeAnd :: (NormalizedPicture -> Bool) -> (NormalizedPicture -> a) -> Components -> Maybe a
findMaybeAnd f g = listToMaybe . findAllAnd f g


{- |
True if image contains exactly these subpictures and nothing else.
-}
containsExactElems :: [NormalizedPicture] -> PicPredicate
containsExactElems ps = specElems (all (\tp -> Pictures ps `contains` tp) . getSubPictures)

{- |
True if image contains at least this subpicture and optionally something else.
-}
containsElem :: NormalizedPicture -> PicPredicate
containsElem p = specElems (`contains` p)

{- |
True if image contains at least these subpictures and optionally something else.
-}
containsElems :: [NormalizedPicture] -> PicPredicate
containsElems ps = specElems (\t -> all (\p -> t `contains` p) ps)


{- |
True if image contains this subpicture exactly this many times.
-}
thisOften :: NormalizedPicture -> Int -> PicPredicate
thisOften p amount = specElems (\ps -> count p ps == amount)


{- |
True if image contains this subpicture at least this many times.
-}
atLeast :: NormalizedPicture -> Int -> PicPredicate
atLeast p amount = specElems (\ps -> count p ps >= amount)


{- |
True if image contains this subpicture at most many times.
-}
atMost :: NormalizedPicture -> Int -> PicPredicate
atMost p amount = specElems (\ps -> count p ps <= amount)


{- |
True if amount of times this subpicture is contained in the image lies in the specified range.
-}
inRangeOf :: NormalizedPicture -> (Int,Int) -> PicPredicate
inRangeOf p (lower,upper) = specElems (\ps -> let occurs = count p ps in occurs >= lower && occurs <= upper)


{- |
Runs the first predicate p, then the second q if p evaluated to `True`.
Does not run q if p evaluates to `False`.
-}
ifThen :: PicPredicate -> PicPredicate -> PicPredicate
ifThen f g comp = not (f comp) || g comp


-- Use a predicate on the list of relative positions
specPosition :: SpatialQuery -> PicPredicate
specPosition f (Components (_,rP)) = f rP


{- |
Evaluates given predicates on a student submission.
-}
evaluatePreds :: [PicPredicate] -> Picture -> Bool
evaluatePreds fs pic = all (`evaluatePred` pic) fs


{- |
Evaluates the given predicate on a student submission.
-}
evaluatePred :: PicPredicate -> Picture -> Bool
evaluatePred f = f . getComponents


{- |
Transforms student submission into spatial `Components` form.
-}
getComponents :: Picture -> Components
getComponents = toRelative . toInterface


{- |
True if image contains the specified spatial relations.
Used with corresponding functions like `CodeWorld.Test.isNorthOf`, `CodeWorld.Test.isLeftOf`, etc.
-}
hasRelation :: SpatialQuery -> PicPredicate
hasRelation = specPosition
