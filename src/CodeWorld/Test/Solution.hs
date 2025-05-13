
module CodeWorld.Test.Solution (
  specElems,
  containsElem,
  containsElems,
  containsExactElems,
  specPosition,
  evaluatePred,
  evaluatePreds,
  isExactly,
  hasExactly,
  hasBroadly,
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
  count
  ) where


import Data.Maybe (listToMaybe)

import CodeWorld.Tasks.Picture (Picture, toInterface)
import CodeWorld.Test.Normalize (NormalizedPicture(..), contains, getSubPictures)
import CodeWorld.Test.Relative (
  Components(..),
  RelativePicSpec(..),
  toRelative,
  )



type PicPredicate = Components -> Bool


-- At least one of arbitrarily many predicates evaluates to True
options :: [PicPredicate] -> PicPredicate
options ps c = any (\p -> p c) ps


-- At least one of two predicates evaluates to True
(<||>) :: PicPredicate -> PicPredicate -> PicPredicate
(<||>) p q c = p c || q c


-- Alias for (<||>)
option :: PicPredicate -> PicPredicate -> PicPredicate
option = (<||>)


-- At least one of the given options satisfies the predicate p.
oneOf :: (a -> PicPredicate) -> [a] ->  PicPredicate
oneOf p = foldr ((<||>) . p) (const False)


-- Use a predicate on the list of sub images
specElems :: (NormalizedPicture -> Bool) -> PicPredicate
specElems f (Components (ps,_)) = f ps


-- return the first picture element satisfying the predicate if it exists. (translation is removed)
findMaybe :: (NormalizedPicture -> Bool) -> Components -> Maybe NormalizedPicture
findMaybe f = listToMaybe . findAll f


-- return all picture elements satisfying the predicate. (translation is removed)
findAll :: (NormalizedPicture -> Bool) -> Components -> [NormalizedPicture]
findAll f (Components (ps,_)) = filter f $ getSubPictures ps


-- return all subpictures satisfying the predicate. (includes translation)
findAllActual :: (NormalizedPicture -> Bool) -> Picture -> [NormalizedPicture]
findAllActual f = filter f . getSubPictures . toInterface


-- return the first subpicture satisfying the predicate if it exists. (includes translation)
findMaybeActual :: (NormalizedPicture -> Bool) -> Picture -> Maybe NormalizedPicture
findMaybeActual f = listToMaybe . findAllActual f


-- find all subpictures satisfying a predicate, then apply a function. (includes translation)
findAllActualAnd :: (NormalizedPicture -> Bool) -> (NormalizedPicture -> a) -> Picture -> [a]
findAllActualAnd f g = map g . findAllActual f


-- find the first subpicture satisfying a predicate, then apply a function if it exists. (includes translation)
findMaybeActualAnd :: (NormalizedPicture -> Bool) -> (NormalizedPicture -> a) -> Picture -> Maybe a
findMaybeActualAnd f g = listToMaybe . findAllActualAnd f g


-- find all picture elements satisfying a predicate, then apply a function. (translation is removed)
findAllAnd :: (NormalizedPicture -> Bool) -> (NormalizedPicture -> a) -> Components -> [a]
findAllAnd f g = map g . findAll f


-- find the first element satisfying a predicate, then apply a function if it exists. (translation is removed)
findMaybeAnd :: (NormalizedPicture -> Bool) -> (NormalizedPicture -> a) -> Components -> Maybe a
findMaybeAnd f g = listToMaybe . findAllAnd f g


-- Input contains exactly these sub pictures
containsExactElems :: [NormalizedPicture] -> PicPredicate
containsExactElems ps = specElems (all (\tp -> Pictures ps `contains` tp) . getSubPictures)

-- Input contains at least this sub picture
containsElem :: NormalizedPicture -> PicPredicate
containsElem p = specElems (`contains` p)

-- Input contains at least these sub pictures
containsElems :: [NormalizedPicture] -> PicPredicate
containsElems ps = specElems (\t -> all (\p -> t `contains` p) ps)


-- Sub picture occurs exactly this many times in submission
thisOften :: NormalizedPicture -> Int -> PicPredicate
thisOften p amount = specElems (\ps -> count p ps == amount)


-- Sub picture occurs at least this many times in submission
atLeast :: NormalizedPicture -> Int -> PicPredicate
atLeast p amount = specElems (\ps -> count p ps >= amount)


-- Sub picture occurs at most this many times in submission
atMost :: NormalizedPicture -> Int -> PicPredicate
atMost p amount = specElems (\ps -> count p ps <= amount)


-- occurrences of the sub picture lie in specified range
inRangeOf :: NormalizedPicture -> (Int,Int) -> PicPredicate
inRangeOf p (lower,upper) = specElems (\ps -> let occurs = count p ps in occurs >= lower && occurs <= upper)


count :: NormalizedPicture -> NormalizedPicture -> Int
count thing inside = minimum $ map singleCount $ getSubPictures thing
  where
    singleCount p = length $ filter (`contains` p) $ getSubPictures inside


-- run a predicate on the input only if another succeeded already
ifThen :: PicPredicate -> PicPredicate -> PicPredicate
ifThen f g comp = not (f comp) || g comp


-- Use a predicate on the list of relative positions
specPosition :: ([RelativePicSpec] -> Bool) -> PicPredicate
specPosition f (Components (_,rP)) = f rP


-- Evaluate all of the given predicates on the student submission
evaluatePreds :: [PicPredicate] -> Picture -> Bool
evaluatePreds fs pic = all (`evaluatePred` pic) fs


-- Evaluate a single predicate on the student submission
evaluatePred :: PicPredicate -> Picture -> Bool
evaluatePred f = f . getComponents


-- Turn input picture into abstract representation
getComponents :: Picture -> Components
getComponents = toRelative . toInterface


-- Input is exactly this relative picture
isExactly :: RelativePicSpec -> PicPredicate
isExactly a = specPosition (==[a])


-- Input contains elements satisfying exact spatial predicate
hasExactly :: RelativePicSpec -> PicPredicate
hasExactly (Is p1 d p2) = specPosition (\rs -> all (`elem` rs) allOfThem)
  where
    allOfThem = [Is x d y | x <- getSubPictures p1, y <- getSubPictures p2]

hasExactly p = specPosition (p `elem`)


-- Input contains elements satisfying the given spatial predicate
hasBroadly :: ([RelativePicSpec] -> Bool) -> PicPredicate
hasBroadly = specPosition
