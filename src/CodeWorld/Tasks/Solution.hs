
module CodeWorld.Tasks.Solution (
  Spec(..),
  spec,
  specElems,
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


specElems :: ([NormalizedPicture] -> Bool) -> Components -> Bool
specElems f (Components (e,_)) = f e


containsExactElems :: [NormalizedPicture] -> Components -> Bool
containsExactElems a = specElems (\c -> all (`elem` c) a)


specPosition :: ([RelativePicSpec] -> Bool) -> Components -> Bool
specPosition f (Components (_,rP)) = f rP


evaluate :: PredSpec -> NormalizedPicture -> Bool
evaluate (PredSpec fs) pic = all (\f -> f $ toRelative pic) fs


isExactly :: RelativePicSpec -> Components -> Bool
isExactly a = specPosition (==[a])


has :: RelativePicSpec -> Components -> Bool
has a = specPosition (a `elem`)
