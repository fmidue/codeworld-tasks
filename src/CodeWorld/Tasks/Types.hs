{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}

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


hue :: Color -> Double
hue (HSL h _ _) = h
hue (RGBA r g b _) = hue (RGB r g b)
hue (RGB r g b)
  | hi - lo < epsilon = 0
  | r == hi && g >= b = (g - b) / (hi - lo) * pi / 3
  | r == hi = (g - b) / (hi - lo) * pi / 3 + 2 * pi
  | g == hi = (b - r) / (hi - lo) * pi / 3 + 2 / 3 * pi
  | otherwise = (r - g) / (hi - lo) * pi / 3 + 4 / 3 * pi
  where
    hi = max r (max g b)
    lo = min r (min g b)
    epsilon = 0.000001
hue _ = 0.5

saturation :: Color -> Double
saturation (HSL _ s _) = s
saturation (RGBA r g b _) = saturation (RGB r g b)
saturation (RGB r g b)
  | hi - lo < epsilon = 0
  | otherwise = (hi - lo) / (1 - abs (hi + lo - 1))
  where
    hi = max r (max g b)
    lo = min r (min g b)
    epsilon = 0.000001
saturation _ = 0.5

luminosity :: Color -> Double
luminosity (HSL _ _ l) = l
luminosity (RGBA r g b _) = luminosity (RGB r g b)
luminosity (RGB r g b) = (lo + hi) / 2
  where
    hi = max r (max g b)
    lo = min r (min g b)
luminosity _ = 0.5

alpha :: Color -> Double
alpha (RGBA _ _ _ a)  = a
alpha (Translucent c) = alpha c / 2
alpha _               = 1
