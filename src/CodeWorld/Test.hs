
module CodeWorld.Test (
  module Abstract,
  module AbsTypes,
  module Animation,
  module API,
  module Sharing,
  module Normalize,
  R.Components,
  module Relative,
  module Solution,
  module Types,
  module VectorSpace,
  Picture(..),
  ReifyPicture(..),
  N.NormalizedPicture,
  normalize,
  reduce,
  reduceNoOrder,
  hasInnerPicture,
  innerPicture,
  isIn,
) where


import CodeWorld.Tasks.API as API
import CodeWorld.Tasks.Types as Types hiding (Color(..))
import CodeWorld.Test.Abstract as Abstract
import CodeWorld.Test.AbsTypes as AbsTypes
import CodeWorld.Test.Animation as Animation
import CodeWorld.Test.Normalize as Normalize hiding (
  NormalizedPicture(..),
  couldHaveTranslation,
  getSubPictures,
  stripTranslation,
  )
import qualified CodeWorld.Test.Normalize as N
import CodeWorld.Test.Relative as Relative hiding (
  Alone,
  Components,
  Is,
  )
import qualified CodeWorld.Test.Relative as R
import CodeWorld.Sharing.Feedback as Sharing
import CodeWorld.Test.Solution as Solution
import CodeWorld.Tasks.VectorSpace as VectorSpace hiding (
  atOriginWithOffset,
  crossProduct,
  dotProduct,
  isRectangle,
  rotationAngle,
  sideLengths,
  )

import CodeWorld.Tasks.Picture (
  Picture(..),
  ReifyPicture(..),
  toInterface,
  hasInnerPicture,
  innerPicture,
  isIn,
  )
import Data.List (sort)


{- |
Convert a `Picture` into a t`CodeWorld.Test.NormalizedPicture`.
This applies a number of simplifications, abstractions and rearrangements on the syntax tree of the image.
The result is a new tree in /canonical/ form.
-}
normalize :: Picture -> N.NormalizedPicture
normalize = toInterface

{- |
Apply `normalize`, then re-concretize the abstracted syntax tree.
The result is a new syntax tree, which draws the same image,
but was simplified and rearranged via `normalize`'s rules.
-}
reduce :: Picture -> Picture
reduce = toConcretePicture . toInterface

{- |
Same as `reduce`,
but also erases information on which subpictures are drawn in front or behind others.
-}
reduceNoOrder :: Picture -> Picture
reduceNoOrder p = case reduce p of
  PRec (Pictures ps) -> PRec $ Pictures $ sort ps
  rp                 -> rp