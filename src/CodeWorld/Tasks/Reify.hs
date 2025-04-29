{-# language DeriveTraversable #-}
{-# language DeriveGeneric #-}
{-# language DeriveAnyClass #-}
{-# language TypeFamilies #-}

module CodeWorld.Tasks.Reify (
  ReifyPicture(..),
  Picture(..),
  share,
  toInterface,
  rectangle,
  thickRectangle,
  solidRectangle,
  circle,
  thickCircle,
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
  thickPolygon,
  solidPolygon,
  lettering,
  styledLettering,
  colored,
  coloured,
  translated,
  scaled,
  dilated,
  rotated,
  reflected,
  clipped,
  pictures,
  (&),
  coordinatePlane,
  codeWorldLogo,
  blank,
  ) where


import Control.DeepSeq                  (NFData)
import Data.Foldable                    (toList)
import Data.IntMap                      (IntMap, Key)
import Data.Reify                       (Graph(..), MuRef(..), reifyGraph)
import Data.Text                        (Text)
import GHC.Generics                     (Generic)
import qualified Data.IntMap            as IM

import qualified CodeWorld.Tasks.API    as API
import CodeWorld.Tasks.Types            (Font, TextStyle, Point, Color)



data ReifyPicture a
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
  | Color Color a
  | Translate Double Double a
  | Scale Double Double a
  | Dilate Double a
  | Rotate Double a
  | Reflect Double a
  | Clip Double Double a
  | Pictures [a]
  | And a a
  | CoordinatePlane
  | Logo
  | Blank
  deriving (Show, Foldable, Eq, Ord, Generic, NFData)


newtype Picture = PRec (ReifyPicture Picture) deriving (Show,Eq,Ord,Generic,NFData)


instance Semigroup Picture where
  (<>) = (&)


instance Monoid Picture where
  mempty  = blank
  mconcat = pictures


rectangle :: Double -> Double -> Picture
rectangle x = PRec . Rectangle x

thickRectangle :: Double -> Double -> Double -> Picture
thickRectangle t x = PRec . ThickRectangle t x

solidRectangle :: Double -> Double -> Picture
solidRectangle x = PRec . SolidRectangle x

circle :: Double -> Picture
circle = PRec . Circle

thickCircle :: Double -> Double -> Picture
thickCircle t = PRec . ThickCircle t

solidCircle :: Double -> Picture
solidCircle = PRec . SolidCircle

arc :: Double -> Double -> Double -> Picture
arc a1 a2 = PRec . Arc a1 a2

sector :: Double -> Double -> Double -> Picture
sector a1 a2 = PRec . Sector a1 a2

thickArc :: Double -> Double -> Double -> Double -> Picture
thickArc t a1 a2 = PRec . ThickArc t a1 a2

curve :: [Point] -> Picture
curve = PRec . Curve

thickCurve :: Double -> [Point] -> Picture
thickCurve t = PRec . ThickCurve t

closedCurve :: [Point] -> Picture
closedCurve = PRec . ClosedCurve

thickClosedCurve :: Double -> [Point] -> Picture
thickClosedCurve t = PRec . ThickClosedCurve t

solidClosedCurve :: [Point] -> Picture
solidClosedCurve = PRec . SolidClosedCurve

polyline :: [Point] -> Picture
polyline = PRec . Polyline

thickPolyline :: Double -> [Point] -> Picture
thickPolyline t = PRec . ThickPolyline t

polygon :: [Point] -> Picture
polygon = PRec . Polygon

thickPolygon :: Double -> [Point] -> Picture
thickPolygon t = PRec . ThickPolygon t

solidPolygon :: [Point] -> Picture
solidPolygon = PRec . SolidPolygon

lettering :: Text -> Picture
lettering = PRec . Lettering

styledLettering :: TextStyle -> Font -> Text -> Picture
styledLettering ts f = PRec . StyledLettering ts f

colored :: Color -> Picture -> Picture
colored c = PRec . Color c

coloured :: Color -> Picture -> Picture
coloured = colored

translated :: Double -> Double -> Picture -> Picture
translated x y = PRec . Translate x y

scaled :: Double -> Double -> Picture -> Picture
scaled x y = PRec . Scale x y

dilated :: Double -> Picture -> Picture
dilated a = PRec . Dilate a

rotated :: Double -> Picture -> Picture
rotated a = PRec . Rotate a

reflected :: Double -> Picture -> Picture
reflected a = PRec . Reflect a

clipped :: Double -> Double -> Picture -> Picture
clipped x y = PRec . Clip x y

pictures :: [Picture] -> Picture
pictures = PRec . Pictures

(&) :: Picture -> Picture -> Picture
a & b = PRec $ And a b

coordinatePlane :: Picture
coordinatePlane = PRec CoordinatePlane

codeWorldLogo :: Picture
codeWorldLogo = PRec Logo

blank :: Picture
blank = PRec Blank


instance MuRef Picture where
  type DeRef Picture = ReifyPicture
  mapDeRef f (PRec body) = case body of
    Color c p             -> Color c <$> f p
    Translate x y p       -> Translate x y <$> f p
    Scale x y p           -> Scale x y <$> f p
    Dilate x p            -> Dilate x <$> f p
    Rotate a p            -> Rotate a <$> f p
    Reflect a p           -> Reflect a <$> f p
    Clip x y p            -> Clip x y <$> f p
    And a b               -> And <$> f a <*> f b
    Pictures ps           -> Pictures <$> traverse f ps
    p                     -> pure $ case p of
      Rectangle x y         -> Rectangle x y
      ThickRectangle t x y  -> ThickRectangle t x y
      SolidRectangle x y    -> SolidRectangle x y
      Circle r              -> Circle r
      ThickCircle t r       -> ThickCircle t r
      SolidCircle r         -> SolidCircle r
      Lettering t           -> Lettering t
      StyledLettering s w t -> StyledLettering s w t
      Curve xs              -> Curve xs
      ThickCurve t xs       -> ThickCurve t xs
      ClosedCurve xs        -> ClosedCurve xs
      SolidClosedCurve xs   -> SolidClosedCurve xs
      ThickClosedCurve xs t -> ThickClosedCurve xs t
      Polygon xs            -> Polygon xs
      SolidPolygon xs       -> SolidPolygon xs
      ThickPolygon xs t     -> ThickPolygon xs t
      Polyline xs           -> Polyline xs
      ThickPolyline xs t    -> ThickPolyline xs t
      Sector a1 a2 r        -> Sector a1 a2 r
      Arc a1 a2 r           -> Arc a1 a2 r
      ThickArc t a1 a2 r    -> ThickArc t a1 a2 r
      CoordinatePlane       -> CoordinatePlane
      Logo                  -> Logo
      Blank                 -> Blank


share :: Picture -> IO (IntMap (ReifyPicture Int), IntMap (ReifyPicture Int))
share d = do
  Graph nodes s <- reifyGraph d
  let universe = IM.fromList nodes
      refs = IM.insertWith (+) s 1 $ foldr (mapInsertWith . toList . snd) mempty nodes
      multiRefs = IM.intersection universe $ IM.filter (>1) refs
      lut = IM.intersection universe refs
  pure (multiRefs, lut)


mapInsertWith :: [Key] -> IntMap Int -> IntMap Int
mapInsertWith b m = Prelude.foldr (\x acc-> IM.insertWith (+) x (1 :: Int) acc) m b


toInterface :: API.Drawable a => Picture -> a
toInterface (PRec p) = case p of
  Rectangle x y -> API.rectangle x y
  ThickRectangle t x y -> API.thickRectangle t x y
  SolidRectangle x y -> API.solidRectangle x y
  Circle r -> API.circle r
  ThickCircle t r -> API.thickCircle t r
  SolidCircle r -> API.solidCircle r
  Polygon ps -> API.polygon ps
  SolidPolygon ps -> API.solidPolygon ps
  ThickPolygon t ps -> API.thickPolygon t ps
  ClosedCurve ps -> API.closedCurve ps
  SolidClosedCurve ps -> API.solidClosedCurve ps
  ThickClosedCurve t ps -> API.thickClosedCurve t ps
  Polyline ps -> API.polyline ps
  ThickPolyline t ps -> API.thickPolyline t ps
  Curve ps -> API.curve ps
  ThickCurve t ps -> API.thickCurve t ps
  Sector a1 a2 r -> API.sector a1 a2 r
  Arc a1 a2 r -> API.arc a1 a2 r
  ThickArc t a1 a2 r -> API.thickArc t a1 a2 r
  Lettering t -> API.lettering t
  StyledLettering ts f t -> API.styledLettering ts f t
  Color c q -> API.colored c $ toInterface q
  Translate x y q -> API.translated x y $ toInterface q
  Scale x y q -> API.scaled x y $ toInterface q
  Dilate fac q -> API.dilated fac $ toInterface q
  Rotate a q -> API.rotated a $ toInterface q
  Reflect a q -> API.reflected a $ toInterface q
  Clip x y q -> API.clipped x y $ toInterface q
  Pictures ps -> API.pictures $ map toInterface ps
  And p1 p2 -> toInterface p1 API.& toInterface p2
  CoordinatePlane -> API.coordinatePlane
  Logo -> API.codeWorldLogo
  Blank -> API.blank
