{-# language PatternSynonyms #-}

{- |
Module exporting all functionality needed for running tests on student submissions.
-}

module CodeWorld.Test (
  -- * Normalized Pictures
  -- ** Data Type
  NormalizedPicture,
  Drawable(..),

  -- ** Helpers for defining NormalizedPictures
  -- $Helpers
  someCircle,
  someSolidCircle,
  someSquare,
  someRectangle,
  someTallRectangle,
  someWideRectangle,
  someSolidSquare,
  someSolidRectangle,
  someTallSolidRectangle,
  someWideSolidRectangle,
  someCurve,
  someSolidCurve,
  someColor,
  rotatedHalf,
  rotatedQuarter,
  rotatedThreeQuarters,
  rotatedUpToFull,
  larger,
  largerX,
  largerY,
  smaller,
  smallerX,
  smallerY,

  -- ** Queries on NormalizedPictures
  -- $NormalizedQueries
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

  -- *** Abstract Representations of CodeWorld Types
  -- $AbsTypes
  Size,
  ShapeKind,
  Angle,
  Factor,
  Position,
  AbsPoint,
  AbsColor,
  isSameColor,
  equalColorCustom,

  -- * Spatial View
  -- $SpatialView
  -- ** Spatial Relations
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

  -- ** Predicates on Components
  Components,
  PicPredicate,
  containsElem,
  containsElems,
  containsExactElems,
  thisOften,
  atLeast,
  atMost,
  inRangeOf,
  hasRelation,
  (<||>),
  option,
  options,
  ifThen,
  oneOf,
  evaluatePred,
  evaluatePreds,
  -- | #queries#

  -- ** Queries on Components
  getComponents,
  findMaybe,
  findMaybeAnd,
  findMaybeActual,
  findMaybeActualAnd,
  findAll,
  findAllAnd,
  findAllActual,
  findAllActualAnd,

  -- * Strict Pictures
  -- $StrictPictures

  -- **Type Internals
  Picture,
  pattern Rectangle,
  pattern ThickRectangle,
  pattern SolidRectangle,
  pattern Circle,
  pattern ThickCircle,
  pattern SolidCircle,
  pattern Polygon,
  pattern SolidPolygon,
  pattern ThickPolygon,
  pattern Polyline,
  pattern ThickPolyline,
  pattern Sector,
  pattern Arc,
  pattern ThickArc,
  pattern Curve,
  pattern ThickCurve,
  pattern ClosedCurve,
  pattern SolidClosedCurve,
  pattern ThickClosedCurve,
  pattern Lettering,
  pattern StyledLettering,
  pattern Color,
  pattern Translate,
  pattern Scale,
  pattern Dilate,
  pattern Rotate,
  pattern Reflect,
  pattern Clip,
  pattern Pictures,
  pattern And,
  pattern CoordinatePlane,
  pattern Logo,
  pattern Blank,

  -- ** Misc. Functions for Pictures
  hasInnerPicture,
  innerPicture,
  isIn,

  -- * Test Utility
  -- | Miscellaneous functions to use in tests.

  -- ** Type Conversions
  normalize,
  toConcretePicture,
  reduce,
  reduceNoOrder,

  -- ** Functions for Point-based Shapes
  -- $PointLists
  wasTranslatedBy,
  wasScaledBy,
  wasRotatedBy,

  -- ** Animation Test Frame Generators
  irregularSamples,
  samplesUntil,

  -- ** CSE detection
  testCSE,

  -- * Re-exports of CodeWorld Interface
  -- ** Math
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

  -- ** Text Rendering Modifiers
  Font(..),
  TextStyle(..),
  ) where


import Data.Text                        (Text)

import CodeWorld.Tasks.API              (Drawable(..))
import CodeWorld.Tasks.Color            (Color)
import CodeWorld.Tasks.Types            (Font(..), TextStyle(..))
import CodeWorld.Test.Abstract (
  larger,
  largerX,
  largerY,
  rotatedHalf,
  rotatedQuarter,
  rotatedThreeQuarters,
  rotatedUpToFull,
  smaller,
  smallerX,
  smallerY,
  someCircle,
  someColor,
  someCurve,
  someRectangle,
  someSolidCircle,
  someSolidCurve,
  someSolidRectangle,
  someSolidSquare,
  someSquare,
  someTallRectangle,
  someTallSolidRectangle,
  someWideRectangle,
  someWideSolidRectangle,
  )
import CodeWorld.Test.AbsTypes (
  Position,
  Size,
  Angle,
  Factor,
  AbsColor,
  ShapeKind,
  AbsPoint,
  isSameColor,
  equalColorCustom,
  )
import CodeWorld.Test.Animation (
  samplesUntil,
  irregularSamples,
  )
import CodeWorld.Test.Normalize (
  NormalizedPicture,

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

  toConcretePicture,
  )
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
import CodeWorld.Sharing.Feedback       (testCSE)
import CodeWorld.Test.Solution (
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
import CodeWorld.Tasks.VectorSpace (
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

  wasTranslatedBy,
  wasScaledBy,
  wasRotatedBy,
  )
import CodeWorld.Tasks.Picture (
  Picture(..),
  toInterface,
  hasInnerPicture,
  innerPicture,
  isIn,
  )
import Data.List (sort)
import qualified CodeWorld.Tasks.Picture as P



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
normalize :: Picture -> NormalizedPicture
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
  PRec (P.Pictures ps) -> PRec $ P.Pictures $ sort ps
  rp                 -> rp

pattern Rectangle :: Double -> Double -> Picture
pattern Rectangle x y <- PRec (P.Rectangle x y)

pattern ThickRectangle :: Double -> Double -> Double -> Picture
pattern ThickRectangle t x y <- PRec (P.ThickRectangle t x y)

pattern SolidRectangle :: Double -> Double -> Picture
pattern SolidRectangle x y <- PRec (P.SolidRectangle x y)

pattern Circle :: Double -> Picture
pattern Circle r <- PRec (P.Circle r)

pattern ThickCircle :: Double -> Double -> Picture
pattern ThickCircle t r <- PRec (P.ThickCircle t r)

pattern SolidCircle :: Double -> Picture
pattern SolidCircle r <- PRec (P.SolidCircle r)

pattern Polygon :: [Point] -> Picture
pattern Polygon ps <- PRec (P.Polygon ps)

pattern SolidPolygon :: [Point] -> Picture
pattern SolidPolygon ps <- PRec (P.SolidPolygon ps)

pattern ThickPolygon :: Double -> [Point] -> Picture
pattern ThickPolygon t ps <- PRec (P.ThickPolygon t ps)

pattern Polyline :: [Point] -> Picture
pattern Polyline ps <- PRec (P.Polyline ps)

pattern ThickPolyline :: Double -> [Point] -> Picture
pattern ThickPolyline t ps <- PRec (P.ThickPolyline t ps)

pattern Sector :: Double -> Double -> Double -> Picture
pattern Sector a1 a2 r <- PRec (P.Sector a1 a2 r)

pattern Arc :: Double -> Double -> Double -> Picture
pattern Arc a1 a2 r <- PRec (P.Arc a1 a2 r)

pattern ThickArc :: Double -> Double -> Double -> Double -> Picture
pattern ThickArc t a1 a2 r <- PRec (P.ThickArc t a1 a2 r)

pattern Curve :: [Point] -> Picture
pattern Curve ps <- PRec (P.Curve ps)

pattern ThickCurve :: Double -> [Point] -> Picture
pattern ThickCurve t ps <- PRec (P.ThickCurve t ps)

pattern ClosedCurve :: [Point] -> Picture
pattern ClosedCurve ps <- PRec (P.ClosedCurve ps)

pattern SolidClosedCurve :: [Point] -> Picture
pattern SolidClosedCurve ps <- PRec (P.SolidClosedCurve ps)

pattern ThickClosedCurve :: Double -> [Point] -> Picture
pattern ThickClosedCurve t ps <- PRec (P.ThickClosedCurve t ps)

pattern Lettering :: Text -> Picture
pattern Lettering t <- PRec (P.Lettering t)

pattern StyledLettering :: TextStyle -> Font -> Text -> Picture
pattern StyledLettering ts f t <- PRec (P.StyledLettering ts f t)

pattern Color :: Color -> Picture -> Picture
pattern Color c p <- PRec (P.Color c p)

pattern Translate :: Double -> Double -> Picture -> Picture
pattern Translate x y p <- PRec (P.Translate x y p)

pattern Scale :: Double -> Double -> Picture -> Picture
pattern Scale fac1 fac2 p <- PRec (P.Scale fac1 fac2 p)

pattern Dilate :: Double -> Picture -> Picture
pattern Dilate fac p <- PRec (P.Dilate fac p)

pattern Rotate :: Double -> Picture -> Picture
pattern Rotate a p <- PRec (P.Rotate a p)

pattern Reflect :: Double -> Picture -> Picture
pattern Reflect a p <- PRec (P.Reflect a p)

pattern Clip :: Double -> Double -> Picture -> Picture
pattern Clip x y p <- PRec (P.Clip x y p)

pattern Pictures :: [Picture] -> Picture
pattern Pictures ps <- PRec (P.Pictures ps)

pattern And :: Picture -> Picture -> Picture
pattern And p1 p2 <- PRec (P.And p1 p2)

pattern CoordinatePlane :: Picture
pattern CoordinatePlane <- PRec P.CoordinatePlane

pattern Logo :: Picture
pattern Logo <- PRec P.Logo

pattern Blank :: Picture
pattern Blank <- PRec P.Blank
