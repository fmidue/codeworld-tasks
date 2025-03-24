
module CodeWorld.Tasks.Picture where


import CodeWorld.Tasks.API              (Drawable(..))
import CodeWorld.Tasks.Types            (Color, Font, TextStyle, Point)
import Data.Text                        (Text)



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
  deriving (Show, Eq, Ord)


instance Drawable Picture where
  rectangle        = Rectangle
  thickRectangle   = ThickRectangle
  solidRectangle   = SolidRectangle
  circle           = Circle
  thickCircle      = ThickCircle
  solidCircle      = SolidCircle
  arc              = Arc
  sector           = Sector
  thickArc         = ThickArc
  curve            = Curve
  thickCurve       = ThickCurve
  closedCurve      = ClosedCurve
  thickClosedCurve = ThickClosedCurve
  solidClosedCurve = SolidClosedCurve
  polyline         = Polyline
  thickPolyline    = ThickPolyline
  polygon          = Polygon
  thickPolygon     = ThickPolygon
  solidPolygon     = SolidPolygon
  lettering        = Lettering
  styledLettering  = StyledLettering
  colored          = Color
  translated       = Translate
  scaled           = Scale
  dilated          = Dilate
  rotated          = Rotate
  reflected        = Reflect
  clipped          = Clip
  pictures         = Pictures
  a & b            = And a b
  coordinatePlane  = CoordinatePlane
  codeWorldLogo    = Logo
  blank            = Blank


toInterface :: Drawable a => Picture -> a
toInterface (Rectangle x y)         = rectangle x y
toInterface (ThickRectangle t x y)  = thickRectangle t x y
toInterface (SolidRectangle x y)    = solidRectangle x y
toInterface (Circle r)              = circle r
toInterface (ThickCircle t r)       = thickCircle t r
toInterface (SolidCircle r)         = solidCircle r
toInterface (Arc a1 a2 r)           = arc a1 a2 r
toInterface (Sector a1 a2 r)        = sector a1 a2 r
toInterface (ThickArc t a1 a2 r)    = thickArc t a1 a2 r
toInterface (Curve a)               = curve a
toInterface (ThickCurve t a)        = thickCurve t a
toInterface (ClosedCurve a)         = closedCurve a
toInterface (ThickClosedCurve t a)  = thickClosedCurve t a
toInterface (SolidClosedCurve a)    = solidClosedCurve a
toInterface (Polyline ps)           = polyline ps
toInterface (ThickPolyline ps t)    = thickPolyline ps t
toInterface (Polygon ps)            = polygon ps
toInterface (ThickPolygon ps t)     = thickPolygon ps t
toInterface (SolidPolygon ps)       = solidPolygon ps
toInterface (Lettering t)           = lettering t
toInterface (StyledLettering a b t) = styledLettering a b t
toInterface (Color c p)             = colored c $ toInterface p
toInterface (Translate x y p)       = translated x y $ toInterface p
toInterface (Scale f1 f2 p)         = scaled f1 f2 $ toInterface p
toInterface (Dilate f p)            = dilated f $ toInterface p
toInterface (Rotate a p)            = rotated a $ toInterface p
toInterface (Reflect a p)           = reflected a $ toInterface p
toInterface (Clip x y p)            = clipped x y $ toInterface p
toInterface (Pictures ps)           = pictures $ map toInterface ps
toInterface (And a b)               = toInterface a & toInterface b
toInterface CoordinatePlane         = coordinatePlane
toInterface Logo                    = codeWorldLogo
toInterface Blank                   = blank