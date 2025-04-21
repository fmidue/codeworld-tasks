{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}

module CodeWorld.Tasks.Types (
  Point,
  Vector,

  Color,
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
import Data.List                        (sort)
import Data.Text                        (Text)
import GHC.Generics                     (Generic)



type Point = (Double,Double)
type Vector = (Double,Double)

data TextStyle
  = Plain
  | Bold
  | Italic
  deriving (Eq,Ord,Show)


data Font
  = SansSerif
  | Serif
  | Monospace
  | Handwriting
  | Fancy
  | NamedFont Text
  deriving (Eq,Ord,Show)


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
  | Modified Color
  | Mixed [Color]
  | RGB Double Double Double
  | HSL Double Double Double
  deriving (Ord,Show,Generic,NFData)

type Colour = Color


instance Eq Color where
  Yellow == Yellow = True
  Green == Green = True
  Red == Red = True
  Blue == Blue = True
  Orange == Orange = True
  Brown == Brown = True
  Pink == Pink = True
  Purple == Purple = True
  Grey == Grey = True
  White == White = True
  Black == Black = True
  Modified c1 == Modified c2 = c1 == c2
  Modified c1 == c2 = c1 == c2
  c1 == Modified c2 = c1 == c2
  Mixed xs == Mixed ys = sort xs == sort ys
  RGB r1 g1 b1 == RGB r2 g2 b2 = r1 == r2 && g1 == g2 && b1 == b2
  HSL h1 s1 l1 == HSL h2 s2 l2 = h1 == h2 && s1 == s2 && l1 == l2
  _ == _ = False


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


oneLayer :: Color -> Color
oneLayer (Modified c) = Modified c
oneLayer c            = Modified c


mixed :: [Color] -> Color
mixed = Mixed

lighter :: Double -> Color -> Color
lighter _ = oneLayer

light :: Color -> Color
light = oneLayer

darker :: Double -> Color -> Color
darker _ = oneLayer

dark :: Color -> Color
dark = oneLayer

brighter :: Double -> Color -> Color
brighter _ = oneLayer

bright :: Color -> Color
bright = oneLayer

duller :: Double -> Color -> Color
duller _ = oneLayer

dull :: Color -> Color
dull = oneLayer

translucent :: Color -> Color
translucent = oneLayer

assortedColors :: [Color]
assortedColors = iterate Modified red


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
