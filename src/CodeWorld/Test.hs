
{- |
Module exporting all functionality needed for running tests on student submissions.
-}

module CodeWorld.Test (
  -- * Helpers for Tests
  module Abstract,

  -- * Abstract Representations of CodeWorld Types
  module AbsTypes,

  -- * Animation Test Frame Generators
  module Animation,

  -- * Drawable API
  module API,

  -- * CSE detection
  module Sharing,

  -- * Normalized Picture Type
  N.NormalizedPicture,
  module Normalize,

  -- * Spatial Relations
  module Relative,

  -- * Test Interface
  module Solution,

  -- * Functions for Point-based Shapes
  V.wasTranslatedBy,
  V.wasScaledBy,
  V.wasRotatedBy,

  -- * Concrete CodeWorld Types
  module Types,

  -- * Math
  module VectorSpace,

  -- * Strict Picture Type Internals
  Picture(..),
  ReifyPicture(..),

  -- * Type Conversions
  normalize,
  N.toConcretePicture,
  reduce,
  reduceNoOrder,

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