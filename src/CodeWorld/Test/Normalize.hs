{-# language OverloadedStrings #-}

module CodeWorld.Test.Normalize (
  Moved(..),
  NormalizedPicture(..),
  contains,
  couldHaveTranslation,
  getExactPos,
  getColor,
  getRotation,
  getScalingFactors,
  getTranslation,
  stripToShape,
  stripTranslation,
  ) where


import Data.List.Extra                  (sort, takeEnd)
import Data.Text                        (Text)
import Data.Tuple.Extra                 (both)

import CodeWorld.Tasks.API              (Drawable(..))
import CodeWorld.Tasks.Types            (Point, Color)
import qualified CodeWorld.Tasks.Types as T
import CodeWorld.Tasks.VectorSpace (
  vectorSum,
  atOriginWithOffset,
  isRectangle,
  reflectedPoint,
  rotationAngle,
  scaledVector,
  sideLengths,
  rotatedVector,
  )



newtype Size = Size Double deriving (Ord)


data Thickness
  = Normal
  | Thick
  deriving (Show,Eq,Ord)


data ShapeKind
  = Hollow Thickness
  | Solid
  deriving (Ord,Show)


data Angle
  = ToQuarter Double
  | ToHalf Double
  | ToThreeQuarter Double
  | ToFull Double
  deriving (Ord)


data Moved
  = Neg Double
  | Pos Double
  | Zero
  deriving (Ord)


data Factor
  = Smaller Double
  | Larger Double
  deriving (Ord)


data AbsColor
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
  | Modified AbsColor
  | Mixed [AbsColor]
  | RGB Double Double Double
  | HSL Double Double Double
  deriving (Ord,Show)



instance Eq AbsColor where
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


toAbsColor :: Color -> AbsColor
toAbsColor color = case color of
  T.Yellow -> Yellow
  T.Green  -> Green
  T.Red    -> Red
  T.Black  -> Black
  T.White  -> White
  T.Blue   -> Blue
  T.Orange -> Orange
  T.Brown  -> Brown
  T.Pink   -> Pink
  T.Purple -> Purple
  T.Grey   -> Grey
  T.Bright c -> withModified c
  T.Brighter _ c -> withModified c
  T.Dull c -> withModified c
  T.Duller _ c -> withModified c
  T.Light c -> withModified c
  T.Lighter _ c -> withModified c
  T.Dark c -> withModified c
  T.Darker _ c -> withModified c
  T.Translucent c -> withModified c
  T.Mixed cs -> Mixed $ map toAbsColor cs
  T.RGB r g b -> RGB r g b
  T.HSL h s l -> HSL h s l
  where
    withModified = Modified . toAbsColor


type AbsPoint = (Moved,Moved)


instance Show Size where
  show _ = "Size"


instance Eq Size where
  _ == _ = True


instance Eq ShapeKind where
  Hollow _ == Hollow _ = True
  Solid    == Solid    = True
  _        == _        = False


instance Show Moved where
  show Zero    = "Zero"
  show (Neg _) = "Neg"
  show (Pos _) = "Pos"

instance Eq Moved where
  (Neg _) == (Neg _) = True
  (Pos _) == (Pos _) = True
  Zero    == Zero    = True
  _       == _       = False


instance Num Moved where
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
  (Smaller _) == (Smaller _) = True
  (Larger _)  == (Larger _)  = True
  _           == _           = False


instance Show Factor where
  show (Smaller _) = "Smaller"
  show (Larger _)  = "Larger"


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


data NormalizedPicture
  = Rectangle !ShapeKind !Size !Size
  | Circle !ShapeKind !Size
  | Lettering !Text
  | Color !AbsColor !NormalizedPicture
  | Translate !Moved !Moved !NormalizedPicture
  | Scale !(Maybe Factor) !(Maybe Factor) !NormalizedPicture
  | Rotate !Angle !NormalizedPicture
  | Pictures [NormalizedPicture]
  | CoordinatePlane
  | Logo
  | Blank
  | Polyline !ShapeKind [AbsPoint]
  | Curve !ShapeKind [AbsPoint]
  | Arc !ShapeKind !Angle !Angle !Size
  | Reflect !Angle !NormalizedPicture
  | Clip !Size !Size !NormalizedPicture
  deriving (Show,Eq,Ord)


thickness :: (Eq a, Fractional a) => a -> Thickness
thickness d
  | d /= 0 = Thick
  | otherwise = Normal


instance Drawable NormalizedPicture where

  pictures [] = blank
  pictures [x] = x
  pictures xs = Pictures xs

  Blank & p = p
  p & Blank = p
  Polyline (Hollow Normal) ps1 & Polyline (Hollow Thick) ps2 =
    Polyline (Hollow Thick) ps2 & Polyline (Hollow Normal) ps1
  Polyline (Hollow t) ps1 & Polyline Solid ps2 =
    Polyline Solid ps2 & Polyline (Hollow t) ps1
  Curve (Hollow Normal) ps1 & Curve (Hollow Thick) ps2 =
    Curve (Hollow Thick) ps2 & Curve (Hollow Normal) ps1
  Curve (Hollow t) ps1 & Curve Solid ps2 =
    Curve Solid ps2 & Curve (Hollow t) ps1
  Polyline sp ps1 & Curve sc ps2 = Curve sc ps2 & Polyline sp ps1
  Polyline s1 ps1 & Polyline s2 ps2 = handleFreeShape True  s1 s2 ps1 ps2
  Curve    s1 ps1 & Curve    s2 ps2 = handleFreeShape False s1 s2 ps1 ps2
  p & Polyline s ps = Polyline s ps & p
  p & Curve s ps = Curve s ps & p
  p1 & p2 = Pictures $ ps1 ++ ps2
    where
      ps1 = case p1 of
        Pictures ps -> ps
        _           -> [p1]
      ps2 = case p2 of
        Pictures ps -> ps
        _           -> [p2]

  blank = Blank

  coordinatePlane = CoordinatePlane
  codeWorldLogo = Logo

  circle 0 = blank
  circle r = Circle (Hollow Normal) $ toSize r

  solidCircle 0 = blank
  solidCircle r = Circle Solid $ toSize r

  thickCircle 0 _ = blank
  thickCircle t r = Circle shape $ toSize (r + t/2)
    where
      shape
        | t == 2*r = Solid
        | otherwise = Hollow $ thickness t

  rectangle 0 _ = blank
  rectangle _ 0 = blank
  rectangle l w  = Rectangle (Hollow Normal) (toSize l) $ toSize w

  solidRectangle 0 _ = blank
  solidRectangle _ 0 = blank
  solidRectangle l w = Rectangle Solid (toSize l) $ toSize w

  thickRectangle _ 0 _ = blank
  thickRectangle _ _ 0 = blank
  thickRectangle t l w = Rectangle shape (toSize $ l + t/2) $ toSize $ w + t/2
    where
      shape
        | t >= 2*l || t >= 2*w = Solid
        | otherwise = Hollow $ thickness t

  arc      = checkForCircle $ Hollow Normal
  sector   = checkForCircle Solid
  thickArc = checkForCircle . Hollow . thickness

  curve            = handlePointList $ Curve $ Hollow Normal
  thickCurve t     = handlePointList $ Curve $ Hollow $ thickness t
  solidClosedCurve = handlePointList (Curve Solid) . toOpenShape

  closedCurve        = curve . toOpenShape
  thickClosedCurve t = thickCurve t . toOpenShape

  polyline        = checkForRectangle $ Hollow Normal
  thickPolyline t = checkForRectangle $ Hollow $ thickness t
  solidPolygon    = checkForRectangle Solid . toOpenShape

  polygon        = polyline . toOpenShape
  thickPolygon t = thickPolyline t . toOpenShape

  lettering "" = blank
  lettering t  = Lettering t

  styledLettering _ _ "" = blank
  styledLettering _ _ t = Lettering t

  translated 0 0 p = p
  translated x y p = case p of
    Translate a b q -> translated (x + getExactPos a) (y + getExactPos b) q
    Pictures ps     -> Pictures $ map (translated x y) ps
    Blank           -> Blank
    Color c q       -> Color c $ translated x y q
    Polyline s ps   -> Polyline s $ map (applyToAbsPoint (vectorSum (x,y))) ps
    Curve s ps      -> Curve    s $ map (applyToAbsPoint (vectorSum (x,y))) ps
    a               -> Translate (toPosition x) (toPosition y) a

  colored c p = case p of
    Color _ q      -> colored c q
    Pictures ps    -> Pictures $ map (colored c) ps
    Blank          -> Blank
    q              -> Color (toAbsColor c) q

  dilated fac = scaled fac fac

  scaled 0 _ _ = blank
  scaled _ 0 _ = blank
  scaled 1 1 p = p
  scaled fac1 fac2 p = case p of
    Scale f1 f2 q    -> scaled (fromFactor f1 * fac1) (fromFactor f2 * fac2) q
    Translate x y q  -> Translate
                         (toPosition $ getExactPos x*fac1)
                         (toPosition $ getExactPos y*fac2)
                         $ scaled fac1 fac2 q
    Blank            -> Blank
    Color c q        -> Color c $ scaled fac1 fac2 q
    Pictures ps      -> Pictures $ map (scaled fac1 fac2) ps
    Polyline s ps    -> Polyline s $ map (applyToAbsPoint (scaledVector fac1 fac2)) ps
    Curve s ps       -> Curve    s $ map (applyToAbsPoint (scaledVector fac1 fac2)) ps
    a                -> Scale (toFactor fac1) (toFactor fac2) a

  rotated a p
    | getExactAngle (toAngle a) == 0 = p
    | otherwise = case p of
      Rotate a2 q     -> rotated (a + getExactAngle a2) q
      Reflect a2 q    -> reflected (getExactAngle a2 + a/2) q
      Translate x y q -> Translate
                          (toPosition $ getExactPos x*cos a - getExactPos y*sin a)
                          (toPosition $ getExactPos x*sin a + getExactPos y*cos a)
                          $ rotated a q
      Color c q       -> Color c $ rotated a q
      Pictures ps     -> Pictures $ map (rotated a) ps
      Polyline s ps   -> Polyline s $ map (applyToAbsPoint (rotatedVector a)) ps
      Curve s ps      -> Curve    s $ map (applyToAbsPoint (rotatedVector a)) ps
      Rectangle s x y |  getExactAngle (toAngle a) == pi -> Rectangle s y x
      Circle s r      -> Circle s r
      q               -> Rotate (toAngle a) q

  reflected a1 (Reflect a2 p)
    | a1 == getExactAngle a2 = p
    | otherwise = rotated (a1*2 - getExactAngle a2*2) p
  reflected a (Rectangle s x y) = rotated (a*2) $ Rectangle s x y
  reflected _ (Circle s r) = Circle s r
  reflected a (Polyline s ps) = Polyline s $ map (applyToAbsPoint (reflectedPoint a)) ps
  reflected a (Curve s ps) = Curve s $ map (applyToAbsPoint (reflectedPoint a)) ps
  reflected a (Pictures ps) = Pictures $ map (reflected a) ps
  reflected a p@(Translate _ y _) = let d = a*2 in
    translated (2*getExactPos y*sin d) (-(2*getExactPos y*cos d)) $ rotated d p
  reflected a (Rotate a2 p) = reflected (a - (getExactAngle a2/2)) p
  reflected a (Color c q)   = Color c $ reflected a q
  reflected a p = Reflect (toAngle a) p

   -- TODO: clip free shapes?
  clipped x y = Clip (toSize x) (toSize y)


checkForCircle :: ShapeKind -> Double -> Double -> Double -> NormalizedPicture
checkForCircle _ _ _ 0 = blank
checkForCircle shape a1 a2 r
  | a1 == a2  = blank
  | a1 > a2 = arc a2 a1 r
  | abs (a1 - a2) >= 2*pi = circleKind r
  | otherwise = Arc shape (toAngle a1) (toAngle a2) (toSize r)
  where
    circleKind = case shape of
      Hollow Normal -> circle
      Hollow Thick  -> thickCircle 1
      Solid         -> solidCircle


checkForRectangle :: ShapeKind -> [Point] -> NormalizedPicture
checkForRectangle shape ps = case pointsToRectangle shape ps of
  Nothing -> handlePointList (Polyline shape) ps
  Just r  -> r


handlePointList :: Drawable a => ([AbsPoint] -> a) -> [Point] -> a
handlePointList f ps
    | length noRepeats < 2 = blank
    | otherwise = f $ map toAbstractPoint noRepeats
  where
    noRepeats = removeDupes ps


toOpenShape :: [Point] -> [Point]
toOpenShape ps = ps ++ take 1 ps


toFactor :: Double -> Maybe Factor
toFactor 1 = Nothing
toFactor x
  | x > 1 = Just $ Larger x
  | otherwise = Just $ Smaller x


fromFactor :: Maybe Factor -> Double
fromFactor Nothing = 1
fromFactor (Just ( Smaller x)) = x
fromFactor (Just (Larger x)) = x


removeDupes :: Eq a => [a] -> [a]
removeDupes (x:y:xs)
  | x == y    =      rec
  | otherwise =  x : rec
  where rec = removeDupes (y:xs)
removeDupes xs = xs


toAngle :: Double -> Angle
toAngle a
  | a < 0 = toAngle (a+2*pi)
  | a <= pi/2 = ToQuarter a
  | a <= pi = ToHalf a
  | a <= 3*pi/2 = ToThreeQuarter a
  | a < 2*pi = ToFull a
  | otherwise = toAngle (a-2*pi)


getExactAngle :: Angle -> Double
getExactAngle (ToQuarter a) = a
getExactAngle (ToHalf a) = a
getExactAngle (ToThreeQuarter a) = a
getExactAngle (ToFull a) = a


getExactPos :: Moved -> Double
getExactPos Zero    = 0
getExactPos (Neg d) = -d
getExactPos (Pos d) =  d


toPosition :: Double -> Moved
toPosition d
  | d == 0 = Zero
  | d < 0 = Neg $ abs d
  | otherwise = Pos d


toAbstractPoint :: Point -> AbsPoint
toAbstractPoint (x,y) = (toPosition x, toPosition y)



applyToAbsPoint :: (Point -> Point) -> AbsPoint -> AbsPoint
applyToAbsPoint f ap = toAbstractPoint $ f (concretePoint ap)


pointsToRectangle :: ShapeKind -> [Point] -> Maybe NormalizedPicture
pointsToRectangle shapeKind ps
  | isRectangle ps = Just $ translated x y $ rotated angle $ shapeToUse xLen yLen
  | otherwise = Nothing
  where
    (xLen,yLen) = sideLengths ps
    angle = rotationAngle originPs
    (originPs,(x,y)) = atOriginWithOffset ps
    shapeToUse = case shapeKind of
      Hollow Normal -> rectangle
      Hollow Thick  -> thickRectangle 1
      Solid         -> solidRectangle


handleFreeShape
  :: Bool
  -> ShapeKind
  -> ShapeKind
  -> [AbsPoint]
  -> [AbsPoint]
  -> NormalizedPicture
handleFreeShape isPolyline s1 s2 ps1 ps2
  | toPoint (takeEnd 1 ps1) == toPoint (take 1 ps2)
    && s1 == s2
  = func s1 (toPoint $ ps1 ++ drop 1 ps2)
  | otherwise = pictures [func s1 (toPoint ps1), func s2 (toPoint ps2)]
    where
      toPoint = map concretePoint
      solidCurveHelper = handlePointList $ Curve Solid
      solidPolylineHelper = checkForRectangle Solid

      func s = case s of
        (Hollow Normal)
          | isPolyline -> polyline
          | otherwise  -> curve
        (Hollow Thick)
          | isPolyline -> thickPolyline 1
          | otherwise  -> thickCurve 1
        Solid
          | isPolyline -> solidPolylineHelper
          | otherwise  -> solidCurveHelper


{-
over-approximated (heavily) for PolyLines
same for Curves, but arc edges may also extend outside the rectangle slightly
=> both may need more accuracy to detect overlap better (or this rough estimate is enough for most pictures)
-}
boundingRect :: Drawable a => [AbsPoint] -> a
boundingRect ps = polygon [(xMax,yMax), (xMin,yMax), (xMin,yMin), (xMax,yMin)]
  where
    coordList = both (map getExactPos) $ unzip ps
    (xMax,yMax) = both maximum coordList
    (xMin,yMin) = both minimum coordList


stripToShape :: NormalizedPicture -> NormalizedPicture
stripToShape (Color _ p) = p
stripToShape p = p


contains :: NormalizedPicture -> NormalizedPicture -> Bool
p `contains` (Pictures ps) = all (contains p) ps
(Pictures ps) `contains` p = any (`contains` p) ps
p `contains` q = p == q || case p of
  Translate x y pic -> case q of
    Translate x2 y2 innerP -> x2 == x && y == y2 && pic `contains` innerP
    _                      -> pic `contains` q
  Rotate a pic -> case q of
    Rotate a2 innerP -> a2 == a && pic `contains` innerP
    _                -> pic `contains` q
  Reflect a pic -> case q of
    Reflect a2 innerP -> a == a2 && pic `contains` innerP
    _                 -> pic `contains` q
  Scale f1 f2 pic -> case q of
    Scale g1 g2 innerP -> f1 == g1 && f2 == g2 && pic `contains` innerP
    _                  -> pic `contains` q
  Color c pic -> case q of
    Color c2 innerP -> c == c2 && pic `contains` innerP
    _               -> pic `contains` q
  _ -> False



toSize :: Double -> Size
toSize = Size


concretePoint :: AbsPoint -> Point
concretePoint = both getExactPos


stripTranslation :: NormalizedPicture -> NormalizedPicture
stripTranslation (Translate _ _ p) = p
stripTranslation (Color c p) = Color c $ stripTranslation p
stripTranslation p                 = p


getTranslation :: NormalizedPicture -> (Moved, Moved)
getTranslation (Translate x y _)   = (x,y)
getTranslation (Polyline _ points) = getTranslation (boundingRect points)
getTranslation (Curve _ points)    = getTranslation (boundingRect points)
getTranslation (Color _ p)         = getTranslation p
getTranslation _                   = (0,0)


couldHaveTranslation :: NormalizedPicture -> Bool
couldHaveTranslation Translate {} = True
couldHaveTranslation Polyline {}  = True
couldHaveTranslation Curve {}     = True
couldHaveTranslation (Color _ (Translate {})) = True
couldHaveTranslation _            = False


getColor :: NormalizedPicture -> Maybe AbsColor
getColor (Color c _) = Just c
getColor _           = Nothing


getScalingFactors :: NormalizedPicture -> (Maybe Factor,Maybe Factor)
getScalingFactors (Scale f1 f2 _)   = (f1,f2)
getScalingFactors (Translate _ _ p) = getScalingFactors p
getScalingFactors (Reflect _ p)     = getScalingFactors p
getScalingFactors (Rotate _ p)      = getScalingFactors p
getScalingFactors (Color _ p)       = getScalingFactors p
getScalingFactors _                 = (Nothing,Nothing)


getRotation :: NormalizedPicture -> Maybe Angle
getRotation (Scale _ _ p)   = getRotation p
getRotation (Translate _ _ p) = getRotation p
getRotation (Reflect _ p)     = getRotation p
getRotation (Color _ p)       = getRotation p
getRotation (Rotate a _)      = Just a
getRotation _                 = Nothing
