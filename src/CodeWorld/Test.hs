
{- |
Module exporting all functionality needed for running tests on student submissions.
-}

module CodeWorld.Test (
  -- * Normalized Pictures
  -- ** Data Type
  AbstractPicture,
  Drawable(..),

  -- ** Helpers for defining AbstractPictures
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

  -- ** Queries on AbstractPictures
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
  Picture (
    Rectangle,
    ThickRectangle,
    SolidRectangle,
    Circle,
    ThickCircle,
    SolidCircle,
    Polygon,
    SolidPolygon,
    ThickPolygon,
    Polyline,
    ThickPolyline,
    Sector,
    Arc,
    ThickArc,
    Curve,
    ThickCurve,
    ClosedCurve,
    SolidClosedCurve,
    ThickClosedCurve,
    Lettering,
    StyledLettering,
    Color,
    Translate,
    Scale,
    Dilate,
    Rotate,
    Reflect,
    Clip,
    Pictures,
    And,
    CoordinatePlane,
    Logo,
    Blank
    ),

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

  -- ** Colour
  Color,
  Colour,
  red,
  green,
  yellow,
  black,
  white,
  blue,
  orange,
  brown,
  pink,
  purple,
  grey,
  gray,
  mixed,
  lighter,
  light,
  darker,
  dark,
  brighter,
  bright,
  duller,
  dull,
  translucent,
  assortedColors,
  hue,
  saturation,
  luminosity,
  alpha,
  ) where


import CodeWorld.Tasks.API              (Drawable(..))
import CodeWorld.Tasks.Color (
  Color,
  Colour,
  red,
  green,
  yellow,
  black,
  white,
  blue,
  orange,
  brown,
  pink,
  purple,
  grey,
  gray,
  mixed,
  lighter,
  light,
  darker,
  dark,
  brighter,
  bright,
  duller,
  dull,
  translucent,
  assortedColors,
  hue,
  saturation,
  luminosity,
  alpha,
  )
import CodeWorld.Tasks.Types            (Font(..), TextStyle(..))
import CodeWorld.Test.AbstractHelpers (
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
import CodeWorld.Test.Abstract (
  AbstractPicture,

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
import CodeWorld.Test.Rewrite           (normalize, reduce, reduceNoOrder)
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
  hasInnerPicture,
  innerPicture,
  isIn,
  )



{- $Helpers
Using the `Drawable` API directly does not make much sense with t`CodeWorld.Test.AbstractPicture`s,
since concrete parameters are abstracted.
The following functions can be used instead to build an image more generally.
-}

{- $NormalizedQueries
Functions for retrieving parameters of an t`CodeWorld.Test.AbstractPicture`.
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
