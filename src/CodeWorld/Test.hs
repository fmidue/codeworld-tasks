
{- |
Module exporting all functionality needed for running tests on student submissions.
-}

module CodeWorld.Test (
  -- * Normalized Pictures
  -- ** Data Type
  N.NormalizedPicture,
  module API,

  -- ** Helpers for defining NormalizedPictures
  -- $Helpers
  module Abstract,

  -- ** Queries on NormalizedPictures
  -- $NormalizedQueries
  module Normalize,

  -- *** Abstract Representations of CodeWorld Types
  -- $AbsTypes
  module AbsTypes,

  -- * Spatial View
  -- $SpatialView
  -- ** Spatial Relations
  module Relative,

  -- ** Predicates on Components
  R.Components,
  module SolutionPred,

  -- | #queries#

  -- ** Queries on Components
  module SolutionQuery,

  -- * Strict Pictures
  -- $StrictPictures

  -- **Type Internals
  Picture(..),
  ReifyPicture(..),

  -- ** Misc. Functions for Pictures
  hasInnerPicture,
  innerPicture,
  isIn,

  -- * Test Utility
  -- | Miscellaneous functions to use in tests.

  -- ** Type Conversions
  normalize,
  N.toConcretePicture,
  reduce,
  reduceNoOrder,

  -- ** Functions for Point-based Shapes
  -- $PointLists
  V.wasTranslatedBy,
  V.wasScaledBy,
  V.wasRotatedBy,

  -- ** Animation Test Frame Generators
  module Animation,

  -- ** CSE detection
  module Sharing,

  -- * Re-exports of CodeWorld Interface
  -- ** Math
  module VectorSpace,

  -- ** Text Rendering Modifiers
  module Types,
) where


import CodeWorld.Tasks.API as API
import CodeWorld.Tasks.Types as Types
import CodeWorld.Test.Abstract as Abstract
import CodeWorld.Test.AbsTypes as AbsTypes (
  Size,
  Position,
  Angle,
  Factor,
  AbsColor,
  ShapeKind,
  AbsPoint,
  isSameColor,
  equalColorCustom,
  )
import CodeWorld.Test.Animation as Animation
import CodeWorld.Test.Normalize as Normalize (
  contains,
  count,
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
import qualified CodeWorld.Test.Relative as R
import CodeWorld.Test.Relative as Relative (
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
import CodeWorld.Test.Solution as SolutionPred (
  PicPredicate,
  containsElem,
  containsElems,
  containsExactElems,
  evaluatePred,
  evaluatePreds,
  hasRelation,
  (<||>),
  option,
  options,
  ifThen,
  thisOften,
  atLeast,
  atMost,
  inRangeOf,
  oneOf,
  )
import CodeWorld.Test.Solution as SolutionQuery (
  findMaybe,
  findAll,
  findAllAnd,
  findMaybeAnd,
  findAllActual,
  findMaybeActual,
  findAllActualAnd,
  findMaybeActualAnd,
  getComponents,
  )
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

{- $NormalizedQueries
Functions for retrieving parameters of a t`CodeWorld.Test.NormalizedPicture`.
For each parameter there is an abstract and a concrete version.
The concrete version simply removes the abstract type wrapper and returns the contained value.

These are meant to be used on non-composite values only (no `&` or `pictures`),
meaning only one value is returned.
Use the functions in [Queries on Components](#queries) if retrieval of all parameters of a specific kind is desired.
-}

{- $AbsTypes
Abstract versions of data types used as parameters in CodeWorld's `Picture` type.
Retain the concrete value, unless stated otherwise, so it can be extracted if necessary.
-}

{- $SpatialView
Aside from normalizing the picture and retrieving (individual) parameters,
we also want to inspect how components are laid out on the canvas
and query their parameters as a whole.
The following data types and functions implement this more /zoomed out/ approach.
-}

{- $PointLists
Point list based shapes like `curve`, `polyline` or `polygon` differ from other basic shapes
in that they can be drawn anywhere instead of only in a fixed position in the origin.
Conventional detection of translation, rotation or scaling is thus not possible.
These functions help remedy this problem by comparing two point lists
to see if a transformation was applied to one to get the other.
-}

{- $StrictPictures
Exposed internals of the student facing `Picture` type.
This is useful if specific attributes can be determined directly from the un-normalized syntax tree.
Primitives from [Uniplate](https://hackage.haskell.org/package/uniplate-1.6.13) can then be employed
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