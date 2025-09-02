
{- |
Module exporting all functionality needed for running tests on student submissions.
-}

module CodeWorld.Test (
  -- * Abstract Representations of CodeWorld Types
  module AbsTypes,

  -- * Animation Test Frame Generators
  module Animation,

  -- * Normalized Picture Type
  N.NormalizedPicture,
  module API,
  module Normalize,

  -- * Helpers for defining NormalizedPictures
  -- $Helpers
  module Abstract,

  -- * Spatial Relations
  module Relative,

  -- * Test Interface
  module Solution,

  -- * Functions for Point-based Shapes
  -- $PointLists
  V.wasTranslatedBy,
  V.wasScaledBy,
  V.wasRotatedBy,

  -- * Concrete CodeWorld Types
  module Types,

  -- * Math
  module VectorSpace,

  -- * Strict Picture Type Internals
  -- $Picture
  Picture(..),
  ReifyPicture(..),

  -- * Type Conversions
  normalize,
  N.toConcretePicture,
  reduce,
  reduceNoOrder,

  -- * CSE detection
  module Sharing,

  -- * Misc. Functions for Pictures
  hasInnerPicture,
  innerPicture,
  isIn,
) where


import CodeWorld.Tasks.API as API
import CodeWorld.Tasks.Types as Types
import CodeWorld.Test.Abstract as Abstract
import CodeWorld.Test.AbsTypes as AbsTypes
import CodeWorld.Test.Animation as Animation
import CodeWorld.Test.Normalize as Normalize (
  contains,
  getColor,
  getRotation,
  getExactRotation,
  getScalingFactors,
  getExactScalingFactors,
  getTranslation,
  getExactTranslation,
  getReflectionAngle,
  getExactReflectionAngle,
  getCircleRadius,
  getExactCircleRadius,
  getRectangleLengths,
  getExactRectangleLengths,
  getExactPointList,
  )
import qualified CodeWorld.Test.Normalize as N
import CodeWorld.Test.Relative as Relative (
  Components,
  RelativePicSpec,
  SpatialQuery,
  isSouthOf,
  isNorthOf,
  isWestOf,
  isEastOf,
  isSouthEastOf,
  isSouthWestOf,
  isNorthEastOf,
  isNorthWestOf,
  isBelow,
  isAbove,
  isLeftOf,
  isRightOf,
  atSamePosition,
  )
import CodeWorld.Sharing.Feedback as Sharing
import CodeWorld.Test.Solution as Solution
import qualified CodeWorld.Tasks.VectorSpace as V
import CodeWorld.Tasks.VectorSpace as VectorSpace (
  Point,
  Vector,
  translatedPoint,
  rotatedPoint,
  reflectedPoint,
  scaledPoint,
  dilatedPoint,
  vectorLength,
  vectorDirection,
  vectorSum,
  vectorDifference,
  scaledVector,
  rotatedVector,
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



{- $Helpers
Using the `Drawable` API directly does not make much sense with t`CodeWorld.Test.NormalizedPicture`s,
since concrete parameters are abstracted.
The following functions can be used instead to build an image more generally.
-}

{- $PointLists
Point list based shapes like `curve`, `polyline` or `polygon` differ from other basic shapes
in that they can be drawn anywhere instead of only in a fixed position in the origin.
Conventional detection of translation, rotation or scaling is thus not possible.
These functions help remedy this problem by comparing two point lists
to see if a transformation was applied to one to get the other.
-}

{- $Picture
Exposed constructors of the student facing Picture type.
This is used if specific attributes can be determined directly from the un-normalized syntax tree.
Primitives from [Uniplate](https://hackage.haskell.org/package/uniplate-1.6.13) can then be used
to generically traverse the structure.
-}

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
reduce = N.toConcretePicture . toInterface

{- |
Same as `reduce`,
but also erases information on which subpictures are drawn in front or behind others.
-}
reduceNoOrder :: Picture -> Picture
reduceNoOrder p = case reduce p of
  PRec (Pictures ps) -> PRec $ Pictures $ sort ps
  rp                 -> rp