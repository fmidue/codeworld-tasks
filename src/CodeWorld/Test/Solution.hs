
module CodeWorld.Test.Solution (
  --Spec(..),
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
  ) where


import CodeWorld.Tasks.Reify (Picture, toInterface)
import CodeWorld.Test.Normalize (NormalizedPicture, contains)
import CodeWorld.Test.Relative (
  Components(..),
  RelativePicSpec,
  toRelative,
  )



--data Spec = Only Components | ReqAndOpt (Components,Components)

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


-- Use a predicate on the list of sub images
specElems :: ([NormalizedPicture] -> Bool) -> PicPredicate
specElems f (Components (ps,_)) = f ps


-- Input contains exactly these sub pictures
containsExactElems :: [NormalizedPicture] -> PicPredicate
containsExactElems ps = specElems (all (`elem` ps))

-- Input contains at least this sub picture
containsElem :: NormalizedPicture -> PicPredicate
containsElem p = specElems (any (`contains` p))

-- Input contains at least these sub pictures
containsElems :: [NormalizedPicture] -> PicPredicate
containsElems ps = specElems (any (or . (\c -> map (c `contains`) ps)))


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


count :: NormalizedPicture -> [NormalizedPicture] -> Int
count thing = length . filter (`contains` thing)


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
evaluatePred f = f . toRelative . toInterface


-- Input is exactly this relative picture
isExactly :: RelativePicSpec -> PicPredicate
isExactly a = specPosition (==[a])


-- Input contains elements satisfying exact spatial predicate
hasExactly :: RelativePicSpec -> PicPredicate
hasExactly a = specPosition (a `elem`)


-- Input contains elements satisfying the given spatial predicate
hasBroadly :: (RelativePicSpec -> Bool) -> PicPredicate
hasBroadly f = specPosition (any f)
