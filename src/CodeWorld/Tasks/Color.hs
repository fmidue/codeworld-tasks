{-# language DeriveAnyClass #-}
{-# language DeriveDataTypeable #-}
{-# language DeriveGeneric #-}
{-# language ViewPatterns #-}

module CodeWorld.Tasks.Color (
  Color(..),
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


import Control.DeepSeq                  (NFData)
import Data.Data                        (Data)
import GHC.Generics                     (Generic)



{-|
Color type mirroring CodeWorld's equivalent type.
The exposed constructors allow for setting colors directly via numerical values.
-}
data Color
  = Yellow
  | Green
  | Red
  | Blue
  | Orange
  | Brown
  | Pink
  | Purple
  | Grey
  | White
  | Black
  | Bright Color
  | Brighter Double Color
  | Dull Color
  | Duller Double Color
  | Light Color
  | Lighter Double Color
  | Dark Color
  | Darker Double Color
  | Translucent Color
  | Mixed [Color]
  | RGB Double Double Double
  -- ^ Values for red, green, blue __given as a percentage from 0 to 1__
  | HSL Double Double Double
  -- ^ Values for hue, saturation and luminosity
  | RGBA Double Double Double Double
  -- ^ RGB with an additional transparency percentage
  | AnyColor
  deriving (Eq,Ord,Show,Generic,NFData,Data)


{-|
Alias for `Color`.
-}
type Colour = Color

{-|
Constant basic colour.
-}
green, red, yellow, black, white, blue, orange, brown, pink, purple, grey :: Color
yellow = Yellow
green  = Green
red    = Red
black = Black
white = White
blue = Blue
orange = Orange
brown = Brown
pink = Pink
purple = Purple
grey = Grey

{-|
Alias for `grey`.
-}
gray :: Color
gray = grey

{-|
Blend a new color from the arguments.
-}
mixed :: [Color] -> Color
mixed = Mixed

{-|
Increase argument's luminosity by a user defined amount.
-}
lighter :: Double -> Color -> Color
lighter = Lighter

{-|
Slightly increase argument's luminosity.
-}
light :: Color -> Color
light = Light

{-|
Decrease argument's luminosity by a user defined amount.
-}
darker :: Double -> Color -> Color
darker = Darker

{-|
Slightly decrease argument's luminosity.
-}
dark :: Color -> Color
dark = Dark

{-|
Increase argument's saturation by a user defined amount.
-}
brighter :: Double -> Color -> Color
brighter = Brighter

{-|
Slightly increase argument's saturation.
-}
bright :: Color -> Color
bright = Bright

{-|
Decrease argument's saturation by a user defined amount.
-}
duller :: Double -> Color -> Color
duller = Duller

{-|
Slightly decrease argument's saturation.
-}
dull :: Color -> Color
dull = Dull

{-|
Slightly increase argument's transparency.
-}
translucent :: Color -> Color
translucent = Translucent


{-|
An infinite list of different colours.
-}
assortedColors :: [Color]
assortedColors = [HSL (adjusted h) 0.75 0.5 | h <- [0, 2 * pi / phi ..]]
  where
    phi = (1 + sqrt 5) / 2
    adjusted x =
      x + a0
        + a1 * sin (1 * x)
        + b1 * cos (1 * x)
        + a2 * sin (2 * x)
        + b2 * cos (2 * x)
        + a3 * sin (3 * x)
        + b3 * cos (3 * x)
        + a4 * sin (4 * x)
        + b4 * cos (4 * x)
    a0 = -8.6870353473225553e-02
    a1 = 8.6485747604766350e-02
    b1 = -9.6564816819163041e-02
    a2 = -3.0072759267059756e-03
    b2 = 1.5048456422494966e-01
    a3 = 9.3179137558373148e-02
    b3 = 2.9002513227535595e-03
    a4 = -6.6275768228887290e-03
    b4 = -1.0451841243520298e-02


innerColor :: Color -> Color
innerColor col = case col of
  Bright c      -> c
  Brighter _ c  -> c
  Dull c        -> c
  Duller _ c    -> c
  Light c       -> c
  Lighter _ c   -> c
  Dark c        -> c
  Darker _ c    -> c
  Translucent c -> c
  _             -> col


clamp :: Double -> Double
clamp = max 0 . min 1


clampColor :: Color -> Color
clampColor (RGBA r g b a) = RGBA (clamp r) (clamp g) (clamp b) (clamp a)
clampColor (RGB r g b) = RGB (clamp r) (clamp g) (clamp b)
clampColor (HSL h s l) = HSL (moduloTwoPi h) (clamp s) (clamp l)
clampColor c = c


moduloTwoPi :: Double -> Double
moduloTwoPi x = x - fromInteger (floor (x / (2*pi))) * 2*pi


{-|
Returns the hue of the argument according to the HSL model.
-}
hue :: Color -> Double
hue (HSL (moduloTwoPi -> h) _ _) = h
hue (RGBA r g b _) = hue (RGB r g b)
hue (clampColor -> RGB r g b)
  | hi - lo < epsilon = 0
  | r == hi && g >= b = (g - b) / (hi - lo) * pi / 3
  | r == hi = (g - b) / (hi - lo) * pi / 3 + 2 * pi
  | g == hi = (b - r) / (hi - lo) * pi / 3 + 2 / 3 * pi
  | otherwise = (r - g) / (hi - lo) * pi / 3 + 4 / 3 * pi
  where
    hi = max r (max g b)
    lo = min r (min g b)
    epsilon = 0.000001
hue (Mixed cs) = hue $ mix cs
hue Orange = 0.61
hue Yellow = 0.98
hue Green = 2.09
hue Blue = 3.84
hue Purple = 4.8
hue Pink = 5.76
hue Brown = 0.52
hue c
  | c `elem` [White, Black, Grey, Red] = 0
  | otherwise = hue $ innerColor c


{-|
Returns the saturation of the argument according to the HSL model.
-}
saturation :: Color -> Double
saturation (HSL _ (clamp -> s) _) = s
saturation (RGBA r g b _) = saturation (RGB r g b)
saturation (clampColor -> RGB r g b)
  | hi - lo < epsilon = 0
  | otherwise = (hi - lo) / (1 - abs (hi + lo - 1))
  where
    hi = max r (max g b)
    lo = min r (min g b)
    epsilon = 0.000001
saturation (Mixed cs) = saturation $ mix cs
saturation (Bright c) = clamp $ saturation c + 0.25
saturation (Brighter d c) = clamp $ saturation c + d
saturation (Dull c) = clamp $ saturation c - 0.25
saturation (Duller d c) = clamp $ saturation c - d
saturation Brown = 0.6
saturation c
  | c `elem` [White, Black, Grey] = 0
  | c `elem` [Red, Orange, Yellow, Green, Blue, Purple, Pink] = 0.75
  | otherwise = saturation $ innerColor c


{-|
Returns the luminosity of the argument according to the HSL model.
-}
luminosity :: Color -> Double
luminosity (HSL _ _ (clamp -> l)) = l
luminosity (RGBA r g b _) = luminosity (RGB r g b)
luminosity (clampColor -> RGB r g b) = (lo + hi) / 2
  where
    hi = max r (max g b)
    lo = min r (min g b)
luminosity (Mixed cs) = luminosity $ mix cs
luminosity (Light c) = clamp $ luminosity c + 0.15
luminosity (Lighter d c) = clamp $ luminosity c + d
luminosity (Dark c) = clamp $ luminosity c - 0.15
luminosity (Darker d c) = clamp $ luminosity c - d
luminosity White = 1
luminosity Black = 0
luminosity Pink = 0.75
luminosity Brown = 0.4
luminosity c
  | c `elem` [Grey, Red, Orange, Yellow, Green, Blue, Purple] = 0.5
  | otherwise = luminosity $ innerColor c


{-|
Returns the transparency of the argument.
-}
alpha :: Color -> Double
alpha (RGBA _ _ _ (clamp -> a))  = a
alpha (Translucent c) = alpha c / 2
alpha (HSL {}) = 1
alpha (RGB {}) = 1
alpha (Mixed cs) = alpha $ mix cs
alpha c
  | c `elem` predefinedColors = 1
  | otherwise = alpha $ innerColor c
  where
    predefinedColors =
      [ Yellow
      , Green
      , Red
      , Black
      , White
      , Blue
      , Orange
      , Brown
      , Pink
      , Purple
      , Grey
      ]


-- taken and slightly adapted from codeworld-api
mix :: [Color] -> Color
mix = go 0 0 0 0 0
  where
    go rr gg bb aa n (c:cs) = let (r, g, b, a) = toRGBA c in
      go (rr + r * r * a) (gg + g * g * a) (bb + b * b * a) (aa + a) (n + 1) cs
    go rr gg bb aa n []
      | aa == 0 = RGBA 0 0 0 0
      | otherwise = RGBA (sqrt (rr / aa)) (sqrt (gg / aa)) (sqrt (bb / aa)) (aa / n)


-- taken and slightly adapted from codeworld-api
toRGBA :: Color -> (Double,Double,Double,Double)
toRGBA (clampColor -> RGB r g b) = (r, b, g, 1)
toRGBA (clampColor -> RGBA r b g a) = (r, b, g, a)
toRGBA (clampColor -> HSL h s l) = (r, g, b, 1)
  where
    m1 = l * 2 - m2
    m2
      | l <= 0.5 = l * (s + 1)
      | otherwise = l + s - l * s
    r = convert m1 m2 (h / 2 / pi + 1 / 3)
    g = convert m1 m2 (h / 2 / pi)
    b = convert m1 m2 (h / 2 / pi - 1 / 3)
    convert m1' m2' h'
      | h' < 0 = convert m1' m2' (h' + 1)
      | h' > 1 = convert m1' m2' (h' - 1)
      | h' * 6 < 1 = m1' + (m2' - m1') * h' * 6
      | h' * 2 < 1 = m2'
      | h' * 3 < 2 = m1' + (m2' - m1') * (2 / 3 - h') * 6
      | otherwise = m1'
toRGBA (Translucent c) = let (r, g, b, a) = toRGBA c in (r, g, b, a/2)
toRGBA c = toRGBA $ HSL (hue c) (saturation c) (luminosity c)
