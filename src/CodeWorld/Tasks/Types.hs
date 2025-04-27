{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language ViewPatterns #-}

module CodeWorld.Tasks.Types (
  Point,
  Vector,

  Color(..),
  Colour,
  TextStyle,
  Font,

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
  gray,
  grey,
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
import Data.Text                        (Text)
import GHC.Generics                     (Generic)



type Point = (Double,Double)
type Vector = (Double,Double)

data TextStyle
  = Plain
  | Bold
  | Italic
  deriving (Eq,Ord,Show,Generic,NFData)


data Font
  = SansSerif
  | Serif
  | Monospace
  | Handwriting
  | Fancy
  | NamedFont Text
  deriving (Eq,Ord,Show,Generic,NFData)


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
  | HSL Double Double Double
  | RGBA Double Double Double Double
  deriving (Eq,Ord,Show,Generic,NFData)

type Colour = Color


green, red, yellow, black, white, blue, orange, brown, pink, purple, gray, grey :: Color
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
gray = grey


mixed :: [Color] -> Color
mixed = Mixed

lighter :: Double -> Color -> Color
lighter = Lighter

light :: Color -> Color
light = Light

darker :: Double -> Color -> Color
darker = Darker

dark :: Color -> Color
dark = Dark

brighter :: Double -> Color -> Color
brighter = Brighter

bright :: Color -> Color
bright = Bright

duller :: Double -> Color -> Color
duller = Duller

dull :: Color -> Color
dull = Dull

translucent :: Color -> Color
translucent = Translucent

assortedColors :: [Color]
assortedColors = iterate bright red


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


alpha :: Color -> Double
alpha (RGBA _ _ _ (clamp -> a))  = a
alpha (Translucent c) = alpha c / 2
alpha _               = 1


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
