
module CodeWorld.Test (
  module Abstract,
  module API,
  module Compare,
  module Normalize,
  module Relative,
  module Solution,
  module Types,
  module VectorSpace,
  Picture(..),
  ReifyPicture(..),
  N.NormalizedPicture,
  normalize,
  hasInnerPicture,
  innerPicture,
) where


import CodeWorld.Tasks.API as API
import CodeWorld.Tasks.Types as Types
import CodeWorld.Test.Abstract as Abstract
import CodeWorld.Test.Normalize as Normalize hiding (NormalizedPicture(..))
import CodeWorld.Test.Normalize as N
import CodeWorld.Test.Relative as Relative
import CodeWorld.Tasks.Compare as Compare
import CodeWorld.Test.Solution as Solution
import CodeWorld.Tasks.VectorSpace as VectorSpace

import CodeWorld.Tasks.Reify (Picture(..), ReifyPicture(..), toInterface, hasInnerPicture, innerPicture)


normalize :: Picture -> N.NormalizedPicture
normalize = toInterface
