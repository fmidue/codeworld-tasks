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


-- Don't know what to do with these. Never seen anyone use them,
-- but could be problematic if this is used in program logic.

hue :: Color -> Double
hue (HSL h _ _) = h
hue _ = 0.5

saturation :: Color -> Double
saturation (HSL _ s _) = s
saturation _ = 0.5

luminosity :: Color -> Double
luminosity (HSL _ _ l) = l
luminosity _ = 0.5

alpha :: Color -> Double
alpha _ = 1
