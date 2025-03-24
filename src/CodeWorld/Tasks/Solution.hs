
module CodeWorld.Tasks.Solution (
  Spec(..),
  spec,
  specElems,
  containsElems,
  containsExactElems,
  specPosition,
  evaluate,
  isExactly,
  has,
  ) where


import CodeWorld.Tasks.Normalize (NormalizedPicture)
import CodeWorld.Tasks.Relative (
  Components(..),
  RelativePicSpec,
  toRelative,
  )



data Spec = Only Components | ReqAndOpt (Components,Components)

newtype PredSpec = PredSpec [Components -> Bool]


spec :: [Components -> Bool] -> PredSpec
spec = PredSpec


-- Use a predicate on the list of sub images
specElems :: ([NormalizedPicture] -> Bool) -> Components -> Bool
specElems f (Components (ps,_)) = f ps


-- Input contains exactly these sub pictures
containsExactElems :: [NormalizedPicture] -> Components -> Bool
containsExactElems ps = specElems (all (`elem` ps))

-- Input contains at least these sub pictures
containsElems :: [NormalizedPicture] -> Components -> Bool
containsElems ps = specElems (\c -> all (`elem` c) ps)

-- Use a predicate on the list of relative positions
specPosition :: ([RelativePicSpec] -> Bool) -> Components -> Bool
specPosition f (Components (_,rP)) = f rP


-- Evaluate all of the given predicates
evaluate :: PredSpec -> NormalizedPicture -> Bool
evaluate (PredSpec fs) pic = all (\f -> f $ toRelative pic) fs


-- Input is exactly this relative picture
isExactly :: RelativePicSpec -> Components -> Bool
isExactly a = specPosition (==[a])


-- Input contains at least this relative picture
has :: RelativePicSpec -> Components -> Bool
has a = specPosition (a `elem`)
