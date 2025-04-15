
module CodeWorld.Test.Solution (
  --Spec(..),
  specElems,
  containsElem,
  containsElems,
  containsExactElems,
  specPosition,
  evaluateSpec,
  isExactly,
  hasExactly,
  hasBroadly,
  (<||>),
  option,
  options,
  ifThen,
  ) where


import CodeWorld.Tasks.Picture (Picture, toInterface)
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


-- run a predicate on the input only if another succeeded already
ifThen :: PicPredicate -> PicPredicate -> PicPredicate
ifThen f g comp = not (f comp) || g comp


-- Use a predicate on the list of relative positions
specPosition :: ([RelativePicSpec] -> Bool) -> PicPredicate
specPosition f (Components (_,rP)) = f rP


-- Evaluate all of the given predicates on the student submission
evaluateSpec :: [PicPredicate] -> Picture -> Bool
evaluateSpec fs pic = all (\f -> f $ toRelative $ toInterface pic) fs


-- Input is exactly this relative picture
isExactly :: RelativePicSpec -> PicPredicate
isExactly a = specPosition (==[a])


-- Input contains at least this relative picture
hasExactly :: RelativePicSpec -> PicPredicate
hasExactly a = specPosition (a `elem`)


-- Input contains elements satisfying the given spatial predicate
hasBroadly :: (RelativePicSpec -> Bool) -> PicPredicate
hasBroadly f = specPosition (any f)
