{-# language RankNTypes #-}

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
import CodeWorld.Test.Normalize         (NormalizedPicture)

import Control.DeepSeq                  (NFData(..))
import Data.Text                        (Text)


newtype Picture = Picture {unPicture :: forall a . API.Drawable a => a}


instance Semigroup Picture where
  (<>) = (&)

instance Monoid Picture where
  mempty  = blank
  mconcat = pictures

instance NFData Picture where
  rnf p = rnf (unPicture p :: NormalizedPicture)


toInterface :: API.Drawable a => Picture -> a
toInterface = unPicture

rectangle :: Double -> Double -> Picture
rectangle x y = Picture $ API.rectangle x y

thickRectangle :: Double -> Double -> Double -> Picture
thickRectangle t x y = Picture $ API.thickRectangle t x y

solidRectangle :: Double -> Double -> Picture
solidRectangle x y = Picture $ API.solidRectangle x y

circle :: Double -> Picture
circle r = Picture $ API.circle r

thickCircle :: Double -> Double -> Picture
thickCircle t r = Picture $ API.thickCircle t r

solidCircle :: Double -> Picture
solidCircle r = Picture $ API.solidCircle r

arc :: Double -> Double -> Double -> Picture
arc a1 a2 r = Picture $ API.arc a1 a2 r

sector :: Double -> Double -> Double -> Picture
sector a1 a2 r = Picture $ API.sector a1 a2 r

thickArc :: Double -> Double -> Double -> Double -> Picture
thickArc t a1 a2 r = Picture $ API.thickArc t a1 a2 r

curve :: [Point] -> Picture
curve ps = Picture $ API.curve ps

thickCurve :: Double -> [Point] -> Picture
thickCurve t ps = Picture $ API.thickCurve t ps

closedCurve :: [Point] -> Picture
closedCurve ps = Picture $ API.closedCurve ps

thickClosedCurve :: Double -> [Point] -> Picture
thickClosedCurve t ps = Picture $ API.thickClosedCurve t ps

solidClosedCurve :: [Point] -> Picture
solidClosedCurve ps = Picture $ API.solidClosedCurve ps

polyline :: [Point] -> Picture
polyline ps = Picture $ API.polyline ps

thickPolyline :: Double -> [Point] -> Picture
thickPolyline t ps = Picture $ API.thickPolyline t ps

polygon :: [Point] -> Picture
polygon ps = Picture $ API.polygon ps

thickPolygon :: Double -> [Point] -> Picture
thickPolygon t ps = Picture $ API.thickPolygon t ps

solidPolygon :: [Point] -> Picture
solidPolygon ps = Picture $ API.solidPolygon ps

lettering :: Text -> Picture
lettering t = Picture $ API.lettering t

styledLettering :: TextStyle -> Font -> Text -> Picture
styledLettering style font t = Picture $ API.styledLettering style font t

colored :: Color -> Picture -> Picture
colored c p = Picture $ API.colored c $ unPicture p

coloured :: Color -> Picture -> Picture
coloured = colored

translated :: Double -> Double -> Picture -> Picture
translated x y p = Picture $ API.translated x y $ unPicture p

scaled :: Double -> Double -> Picture -> Picture
scaled f1 f2 p = Picture $ API.scaled f1 f2 $ unPicture p

dilated :: Double -> Picture -> Picture
dilated f p = Picture $ API.dilated f $ unPicture p

rotated :: Double -> Picture -> Picture
rotated a p = Picture $ API.rotated a $ unPicture p

reflected :: Double -> Picture -> Picture
reflected a p = Picture $ API.reflected a $ unPicture p

clipped :: Double -> Double -> Picture -> Picture
clipped x y p = Picture $ API.clipped x y $ unPicture p

pictures :: [Picture] -> Picture
pictures ps = Picture $ API.pictures $ map unPicture ps

(&) :: Picture -> Picture -> Picture
infixr 0 &
a & b = Picture $ unPicture a API.& unPicture b

coordinatePlane :: Picture
coordinatePlane = Picture API.coordinatePlane

codeWorldLogo :: Picture
codeWorldLogo = Picture API.codeWorldLogo

blank :: Picture
blank = Picture API.blank
