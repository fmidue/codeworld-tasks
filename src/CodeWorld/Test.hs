
module CodeWorld.Test (
  module Abstract,
  module AbsTypes,
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
  hasInnerPicture,
  innerPicture,
) where


import CodeWorld.Tasks.API as API
import CodeWorld.Tasks.Types as Types hiding (Color(..))
import CodeWorld.Test.Abstract as Abstract
import CodeWorld.Test.AbsTypes as AbsTypes
import CodeWorld.Test.Normalize as Normalize hiding (NormalizedPicture(..))
import CodeWorld.Test.Normalize as N
import CodeWorld.Test.Relative as Relative
import CodeWorld.Sharing.Feedback as Sharing
import CodeWorld.Test.Solution as Solution
import CodeWorld.Tasks.VectorSpace as VectorSpace

import CodeWorld.Tasks.Picture (
  Picture(..),
  ReifyPicture(..),
  toInterface,
  hasInnerPicture,
  innerPicture
  )


normalize :: Picture -> N.NormalizedPicture
normalize = toInterface
