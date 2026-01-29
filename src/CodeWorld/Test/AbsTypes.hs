{-# language DeriveDataTypeable #-}
{-# language ViewPatterns #-}

module CodeWorld.Test.AbsTypes (
  Size,
  Angle,
  Factor(..),
  Position(..),
  AbsColor(..),
  ShapeKind(..),
  Thickness(..),
  AbsPoint,
  toFactor,
  fromFactor,
  toPosition,
  fromPosition,
  toAngle,
  fromAngle,
  toAbsPoint,
  fromAbsPoint,
  fromSize,
  toSize,
  toAbsColor,
  fromAbsColor,
  isSameColor,
  equalColorCustom,
  applyToAbsPoint,
  thickness,
) where


import Data.Data                        (Data)
import Data.Tuple.Extra                 (both)

import CodeWorld.Tasks.Color            (Color)
import CodeWorld.Tasks.VectorSpace      (Point)
import qualified CodeWorld.Tasks.Color  as T



{- |
Abstract representation of radii and side lengths.
All values are considered equal.
-}
newtype Size = Size Double deriving (Ord,Data)

{- |
Abstract representation of points in 2D space.
All values are considered equal.
-}
newtype AbsPoint = AbsPoint {unAbsPoint :: (Position,Position)} deriving (Ord,Show,Data)


{- |
Abstract representation of line width.
Differentiates between standard line width and custom thickness.
-}
data Thickness
  = Normal
  | Thick
  deriving (Show,Eq,Ord,Data)


{- |
Abstract representation of line drawing and filling mode.
Differentiates between the three common shapes: standard, thick, filled.

This does not retain the concrete value.
-}
data ShapeKind
  = Hollow Thickness
  | Solid
  deriving (Ord,Show,Data)


{- |
Abstract representation of angles.
Rotation is divided into four equal sections on the unit circle.
-}
data Angle
  = ToQuarter Double
  | ToHalf Double
  | ToThreeQuarter Double
  | ToFull Double
  deriving (Ord,Data)


{- |
Abstract representation of translations.
Considers values equal if
both are translated by a positive or negative number or not at all.
-}
data Position
  = Neg Double
  | Zero
  | Pos Double
  deriving (Ord,Data)


{- |
Abstract representation of scaling factors.
Considers values equal if
both are scaled larger, smaller or not at all.
-}
data Factor
  = Smaller Double
  | Same
  | Larger Double
  deriving (Ord,Data)


{- |
Abstract representation of `Color`.
Equal if each HSLA parameter has an acceptable distance from those of the other color.
-}
data AbsColor
  = Tone Double Double Double
  -- ^ Represents concrete `Color`'s HSL value
  | Translucent Double AbsColor
  -- ^ Transparency modifier
  | AnyColor
  -- ^ Is equal to any color when compared.
  deriving (Ord,Show,Data)


instance Eq AbsColor where
  Tone h1 s1 l1      == Tone h2 s2 l2
    -- Luminosity at extremes => almost pure white/black
    | (l2 >= 0.981 && l1 >= 0.981) ||
      (l2 <= 0.051 && l1 <= 0.051) = True
    -- Saturation extremely low => almost pure grey
    | s1 <= 0.051 && s2 <= 0.051  = lDiff <= 0.301
    -- Same hue and non-extreme luminosity/saturation => allow for larger range
    | h1 == h2                    = sDiff <= 0.51 && lDiff <= 0.251
    -- Difference of hsl values is in certain range (hue range depends on saturation)
    | otherwise                   =
      hDiff <= 0.151 + (0.1*hueMod) && sDiff <= 0.251 && lDiff <= 0.151
    where
      lDiff = abs (l1 - l2)
      sDiff = abs (s1 - s2)
      hDiff = abs (h1 - h2)
      hueMod = 1 - min s1 s2 - sDiff

  Translucent a1 c1 == Translucent a2 c2 = abs (a1 - a2) <= 0.751 && c1 == c2
  Translucent a c1  == c                 = a >= 0.249 && c1 == c
  c                 == t@Translucent {}  = t == c
  AnyColor          == _                 = True
  _                 == AnyColor          = True


{- |
Abstract a concrete color.
-}
toAbsColor :: Color -> AbsColor
toAbsColor T.AnyColor          = AnyColor
toAbsColor (T.RGB 1   1   1  ) = Tone 0 0 1
toAbsColor (T.RGB 0   0   0  ) = Tone 0 0 0
toAbsColor (T.RGB 0.5 0.5 0.5) = Tone 0 0 0.5
toAbsColor c
  | T.alpha c == 1 = Tone (T.hue c) (T.saturation c) (T.luminosity c)
  | otherwise      = Translucent (T.alpha c) $ Tone (T.hue c) (T.saturation c) (T.luminosity c)


{- |
Concretize an abstract color.
-}
fromAbsColor :: AbsColor -> Color
fromAbsColor (Tone h s l) = T.HSL h s l
fromAbsColor (Translucent _ c) = T.Translucent $ fromAbsColor c
fromAbsColor AnyColor = T.AnyColor


{- |
A more strict equality test for `AbsColor`.
Contrary to the `Eq` instance,
this only succeeds if both colors are completely identical in their HSLA values.
-}
isSameColor :: AbsColor -> AbsColor -> Bool
isSameColor (Tone h1 s1 l1)      (Tone h2 s2 l2)      =
  h1 == h2 && s1 == s2 && l1 == l2
isSameColor (Translucent a1 c1) (Translucent a2 c2) =
  a1 == a2 && c1 `isSameColor` c2
isSameColor AnyColor            _                   = True
isSameColor _                   AnyColor            = True
isSameColor _                   _                   = False


{- |
Allows for custom thresholds on color similarity detection.
This is exported to be able to correct unexpected complications in live tests.
-}
equalColorCustom :: Double -> Double -> Double -> Double -> AbsColor -> AbsColor -> Bool
equalColorCustom hRange sRange lRange _ (Tone h1 s1 l1) (Tone h2 s2 l2)
    | (l2 >= 0.98 && l1 >= 0.98) ||
      (l2 <= 0.05 && l1 <= 0.05) = True
    | s1 <= 0.05 && s2 <= 0.05    = lDiff <= lRange
    | otherwise                   =
      hDiff <= hRange && sDiff <= sRange && lDiff <= lRange
    where
      lDiff = abs (l1 - l2)
      sDiff = abs (s1 - s2)
      hDiff = abs (h1 - h2)
equalColorCustom h s l aRange (Translucent a1 c1) (Translucent a2 c2) =
  abs (a1 - a2) <= aRange && equalColorCustom h s l aRange c1 c2
equalColorCustom h s l aRange (Translucent a c1)  c                   =
  a <= aRange && equalColorCustom h s l aRange c1 c
equalColorCustom h s l aRange c                   (Translucent a c1)  =
  a <= aRange && equalColorCustom h s l aRange c1 c
equalColorCustom _ _ _ _      AnyColor            _                   = True
equalColorCustom _ _ _ _      _                   AnyColor            = True


instance Eq AbsPoint where
  _ == _ = True


instance Show Size where
  show _ = "Size"


instance Eq Size where
  _ == _ = True


instance Eq ShapeKind where
  Hollow _ == Hollow _ = True
  Solid    == Solid    = True
  _        == _        = False


instance Show Position where
  show Zero    = "Zero"
  show (Neg _) = "Neg"
  show (Pos _) = "Pos"

instance Eq Position where
  (Neg _) == (Neg _) = True
  (Pos _) == (Pos _) = True
  Zero    == Zero    = True
  _       == _       = False


instance Num Position where
  Zero + a = a
  a + Zero = a
  Pos a + Pos b = Pos $ a+b
  Neg a + Neg b = Neg $ a+b
  Pos a + Neg b
    | a==b = Zero
    | a < b = Neg $ b-a
    | otherwise = Pos $ a-b
  a@(Neg _) + b@(Pos _) = b + a

  abs Zero = Zero
  abs (Neg a) = Pos a
  abs a = a

  Zero * _ = Zero
  _ * Zero = Zero
  Pos a * Pos b = Pos $ a*b
  Neg a * Neg b = Pos $ a*b
  Pos a * Neg b = Neg $ a*b
  a * b = b*a

  signum Zero = Zero
  signum (Pos _) = Pos 1
  signum (Neg _) = Neg 1

  negate Zero = Zero
  negate (Pos a) = Neg a
  negate (Neg a) = Pos a

  fromInteger i
    | i == 0 = Zero
    | i < 0 = Neg $ fromIntegral i
    | otherwise = Pos $ fromIntegral i


instance Eq Factor where
  Smaller _ == Smaller _ = True
  Larger _  == Larger _  = True
  Same      == Same      = True
  _         == _         = False


instance Show Factor where
  show (Smaller _) = "Smaller"
  show (Larger _)  = "Larger"
  show Same        = "Same"


instance Eq Angle where
  (ToQuarter _)      == (ToQuarter _)      = True
  (ToHalf _)         == (ToHalf _)         = True
  (ToThreeQuarter _) == (ToThreeQuarter _) = True
  (ToFull _)         == (ToFull _)         = True
  _                  == _                  = False


instance Show Angle where
  show (ToQuarter _) = "NoneToQuarter"
  show (ToHalf _) = "QuarterToHalf"
  show (ToThreeQuarter _) = "HalfToThreeQuarter"
  show (ToFull _) = "ThreeQuarterToFull"



{- |
Abstract a concrete line width.
-}
thickness :: (Eq a, Fractional a) => a -> Thickness
thickness d
  | d /= 0 = Thick
  | otherwise = Normal


{- |
Abstract a concrete size.
-}
toSize :: Double -> Size
toSize = Size . abs


{- |
Concretize an abstract size.
-}
fromSize :: Size -> Double
fromSize (Size d) = d


{- |
Abstract a concrete factor.
-}
toFactor :: Double -> Factor
toFactor (abs -> 1) = Same
toFactor (abs -> x)
  | x > 1 = Larger x
  | otherwise = Smaller x


{- |
Concretize an abstract factor.
-}
fromFactor :: Factor -> Double
fromFactor Same = 1
fromFactor (Smaller x) = x
fromFactor (Larger x) = x


{- |
Abstract a concrete angle.
-}
toAngle :: Double -> Angle
toAngle a
  | a < 0 = toAngle (a+2*pi)
  | a <= pi/2 = ToQuarter a
  | a <= pi = ToHalf a
  | a <= 3*pi/2 = ToThreeQuarter a
  | a < 2*pi = ToFull a
  | otherwise = toAngle (a-2*pi)


{- |
Concretize an abstract angle.
-}
fromAngle :: Angle -> Double
fromAngle (ToQuarter a) = a
fromAngle (ToHalf a) = a
fromAngle (ToThreeQuarter a) = a
fromAngle (ToFull a) = a


{- |
Concretize an abstract translation.
-}
fromPosition :: Position -> Double
fromPosition Zero    = 0
fromPosition (Neg d) = fuzz $ -d
fromPosition (Pos d) = fuzz d


fuzz :: Double -> Double
fuzz a = if abs a < 0.005 then 0 else a


{- |
Abstract a concrete translation.
-}
toPosition :: Double -> Position
toPosition d
  | d == 0 = Zero
  | d < 0 = Neg $ abs d
  | otherwise = Pos d


{- |
Concretize an abstract point.
-}
fromAbsPoint :: AbsPoint -> Point
fromAbsPoint = both fromPosition . unAbsPoint


{- |
Abstract a concrete point.
-}
toAbsPoint :: Point -> AbsPoint
toAbsPoint (x,y) = AbsPoint (toPosition x, toPosition y)


{- |
Apply a function on concrete points to the contents of an abstract point.
-}
applyToAbsPoint :: (Point -> Point) -> AbsPoint -> AbsPoint
applyToAbsPoint f ap = toAbsPoint $ f (fromAbsPoint ap)
