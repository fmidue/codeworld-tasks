
module CodeWorld.Test.Solution (
  --Spec(..),
  specElems,
  containsElems,
  containsExactElems,
  specPosition,
  evaluateSpec,
  isExactly,
  hasExact,
  hasApprox,
  (<||>),
  option,
  options,
  ) where


import CodeWorld.Tasks.Picture (Picture, toInterface)
import CodeWorld.Test.Normalize (NormalizedPicture, contains)
import CodeWorld.Test.Relative (
  Components(..),
  RelativePicSpec,
  toRelative,
  )



--data Spec = Only Components | ReqAndOpt (Components,Components)

type PredSpec = [Components -> Bool]


-- At least one of arbitrarily many predicates evaluates to True
options :: [Components -> Bool] -> Components -> Bool
options ps c = any (\p -> p c) ps


-- At least one of two predicates evaluates to True
(<||>) :: (Components -> Bool) -> (Components -> Bool) -> Components -> Bool
(<||>) p q c = p c || q c


-- Alias for (<||>)
option :: (Components -> Bool) -> (Components -> Bool) -> Components -> Bool
option = (<||>)


-- Use a predicate on the list of sub images
specElems :: ([NormalizedPicture] -> Bool) -> Components -> Bool
specElems f (Components (ps,_)) = f ps


-- Input contains exactly these sub pictures
containsExactElems :: [NormalizedPicture] -> Components -> Bool
containsExactElems ps = specElems (all (`elem` ps))

-- Input contains at least these sub pictures
containsElems :: [NormalizedPicture] -> Components -> Bool
containsElems ps = specElems (any (or . (\c -> map (c `contains`) ps)))

-- Use a predicate on the list of relative positions
specPosition :: ([RelativePicSpec] -> Bool) -> Components -> Bool
specPosition f (Components (_,rP)) = f rP


-- Evaluate all of the given predicates on the student submission
evaluateSpec :: PredSpec -> Picture -> Bool
evaluateSpec fs pic = all (\f -> f $ toRelative $ toInterface pic) fs


-- Input is exactly this relative picture
isExactly :: RelativePicSpec -> Components -> Bool
isExactly a = specPosition (==[a])


-- Input contains at least this relative picture
hasExact :: RelativePicSpec -> Components -> Bool
hasExact a = specPosition (a `elem`)


-- Input contains elements satisfying the given spatial predicate
hasApprox :: (RelativePicSpec -> Bool) -> Components -> Bool
hasApprox f = specPosition (any f)
