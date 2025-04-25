
module CodeWorld.Test (
  module Abstract,
  module API,
  module Compare,
  module Normalize,
  module Relative,
  module Solution,
  module Types,
  Picture,
  normalize,
) where


import CodeWorld.Tasks.API as API
import CodeWorld.Tasks.Types as Types
import CodeWorld.Test.Abstract as Abstract
import CodeWorld.Test.Normalize as Normalize
import CodeWorld.Test.Relative as Relative
import CodeWorld.Tasks.Compare as Compare
import CodeWorld.Test.Solution as Solution

import CodeWorld.Tasks.Reify (Picture, toInterface)


normalize :: Picture -> Normalize.NormalizedPicture
normalize = toInterface
