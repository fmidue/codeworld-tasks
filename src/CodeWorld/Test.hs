
module CodeWorld.Test (
  module Abstract,
  module AbsTypes,
  module Animation,
  module API,
  module Sharing,
  module Normalize,
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
import CodeWorld.Test.Normalize as Normalize hiding (NormalizedPicture(..))
import qualified CodeWorld.Test.Normalize as N
import CodeWorld.Test.Relative as Relative
import CodeWorld.Sharing.Feedback as Sharing
import CodeWorld.Test.Solution as Solution
import CodeWorld.Tasks.VectorSpace as VectorSpace

import CodeWorld.Tasks.Picture (
  Picture(..),
  ReifyPicture(..),
  toInterface,
  hasInnerPicture,
  innerPicture,
  isIn,
  )
import Data.List (sort)


-- Normalize the picture
normalize :: Picture -> N.NormalizedPicture
normalize = toInterface

-- Normalize the picture then convert back into an un-normalized syntax tree
reduce :: Picture -> Picture
reduce = toConcretePicture . toInterface

-- reduce and also ignore canvas order
reduceNoOrder :: Picture -> Picture
reduceNoOrder p = case reduce p of
  PRec (Pictures ps) -> PRec $ Pictures $ sort ps
  rp                 -> rp