{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}

module CodeWorld.Tasks.Picture (
  Picture,
  toInterface,
  rectangle,
  solidRectangle,
  thickRectangle,
  circle,
  solidCircle,
  arc,
  sector,
  thickArc,
  curve,
  thickCurve,
  closedCurve,
  thickClosedCurve,
  solidClosedCurve,
  polyline,
  thickPolyline,
  polygon,
  solidPolygon,
  thickPolygon,
  lettering,
  styledLettering,
  thickCircle,
  translated,
  colored,
  coloured,
  dilated,
  scaled,
  rotated,
  reflected,
  clipped,
  pictures,
  (&),
  coordinatePlane,
  codeWorldLogo,
  blank,
) where


import qualified CodeWorld.Tasks.API    as API
import CodeWorld.Tasks.Types            (Color, Font, TextStyle, Point)

import Control.DeepSeq                  (NFData)
import Data.Text                        (Text)
import GHC.Generics                     (Generic)


data Picture
  = Rectangle Double Double
  | ThickRectangle Double Double Double
  | SolidRectangle Double Double
  | Circle Double
  | ThickCircle Double Double
  | SolidCircle Double
  | Polygon [Point]
  | SolidPolygon [Point]
  | ThickPolygon Double [Point]
  | Polyline [Point]
  | ThickPolyline Double [Point]
  | Sector Double Double Double
  | Arc Double Double Double
  | ThickArc Double Double Double Double
  | Curve [Point]
  | ThickCurve Double [Point]
  | ClosedCurve [Point]
  | SolidClosedCurve [Point]
  | ThickClosedCurve Double [Point]
  | Lettering Text
  | StyledLettering TextStyle Font Text
  | Color Color Picture
  | Translate Double Double Picture
  | Scale Double Double Picture
  | Dilate Double Picture
  | Rotate Double Picture
  | Reflect Double Picture
  | Clip Double Double Picture
  | Pictures [Picture]
  | And Picture Picture
  | CoordinatePlane
  | Logo
  | Blank
  deriving (Show, Eq, Ord, Generic, NFData)


rectangle :: Double -> Double -> Picture
rectangle = Rectangle

thickRectangle :: Double -> Double -> Double -> Picture
thickRectangle = ThickRectangle

solidRectangle :: Double -> Double -> Picture
solidRectangle = SolidRectangle

circle :: Double -> Picture
circle = Circle

thickCircle :: Double -> Double -> Picture
thickCircle = ThickCircle

solidCircle :: Double -> Picture
solidCircle = SolidCircle

arc :: Double -> Double -> Double -> Picture
arc = Arc

sector :: Double -> Double -> Double -> Picture
sector = Sector

thickArc :: Double -> Double -> Double -> Double -> Picture
thickArc = ThickArc

curve :: [Point] -> Picture
curve = Curve

thickCurve :: Double -> [Point] -> Picture
thickCurve = ThickCurve

closedCurve :: [Point] -> Picture
closedCurve = ClosedCurve

thickClosedCurve :: Double -> [Point] -> Picture
thickClosedCurve = ThickClosedCurve

solidClosedCurve :: [Point] -> Picture
solidClosedCurve = SolidClosedCurve

polyline :: [Point] -> Picture
polyline = Polyline

thickPolyline :: Double -> [Point] -> Picture
thickPolyline = ThickPolyline

polygon :: [Point] -> Picture
polygon = Polygon

thickPolygon :: Double -> [Point] -> Picture
thickPolygon = ThickPolygon

solidPolygon :: [Point] -> Picture
solidPolygon = SolidPolygon

lettering :: Text -> Picture
lettering = Lettering

styledLettering :: TextStyle -> Font -> Text -> Picture
styledLettering = StyledLettering

colored :: Color -> Picture -> Picture
colored = Color

coloured :: Color -> Picture -> Picture
coloured = colored

translated :: Double -> Double -> Picture -> Picture
translated = Translate

scaled :: Double -> Double -> Picture -> Picture
scaled = Scale

dilated :: Double -> Picture -> Picture
dilated = Dilate

rotated :: Double -> Picture -> Picture
rotated = Rotate

reflected :: Double -> Picture -> Picture
reflected = Reflect

clipped :: Double -> Double -> Picture -> Picture
clipped = Clip

pictures :: [Picture] -> Picture
pictures = Pictures

(&) :: Picture -> Picture -> Picture
a & b = And a b

coordinatePlane :: Picture
coordinatePlane = CoordinatePlane

codeWorldLogo :: Picture
codeWorldLogo = Logo

blank :: Picture
blank = Blank


toInterface :: API.Drawable a => Picture -> a
toInterface (Rectangle x y)         = API.rectangle x y
toInterface (ThickRectangle t x y)  = API.thickRectangle t x y
toInterface (SolidRectangle x y)    = API.solidRectangle x y
toInterface (Circle r)              = API.circle r
toInterface (ThickCircle t r)       = API.thickCircle t r
toInterface (SolidCircle r)         = API.solidCircle r
toInterface (Arc a1 a2 r)           = API.arc a1 a2 r
toInterface (Sector a1 a2 r)        = API.sector a1 a2 r
toInterface (ThickArc t a1 a2 r)    = API.thickArc t a1 a2 r
toInterface (Curve a)               = API.curve a
toInterface (ThickCurve t a)        = API.thickCurve t a
toInterface (ClosedCurve a)         = API.closedCurve a
toInterface (ThickClosedCurve t a)  = API.thickClosedCurve t a
toInterface (SolidClosedCurve a)    = API.solidClosedCurve a
toInterface (Polyline ps)           = API.polyline ps
toInterface (ThickPolyline ps t)    = API.thickPolyline ps t
toInterface (Polygon ps)            = API.polygon ps
toInterface (ThickPolygon ps t)     = API.thickPolygon ps t
toInterface (SolidPolygon ps)       = API.solidPolygon ps
toInterface (Lettering t)           = API.lettering t
toInterface (StyledLettering a b t) = API.styledLettering a b t
toInterface (Color c p)             = API.colored c $ toInterface p
toInterface (Translate x y p)       = API.translated x y $ toInterface p
toInterface (Scale f1 f2 p)         = API.scaled f1 f2 $ toInterface p
toInterface (Dilate f p)            = API.dilated f $ toInterface p
toInterface (Rotate a p)            = API.rotated a $ toInterface p
toInterface (Reflect a p)           = API.reflected a $ toInterface p
toInterface (Clip x y p)            = API.clipped x y $ toInterface p
toInterface (Pictures ps)           = API.pictures $ map toInterface ps
toInterface (And a b)               = toInterface a API.& toInterface b
toInterface CoordinatePlane         = API.coordinatePlane
toInterface Logo                    = API.codeWorldLogo
toInterface Blank                   = API.blank