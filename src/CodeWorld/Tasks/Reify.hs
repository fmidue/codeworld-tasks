{-# language DeriveTraversable #-}
{-# language DeriveGeneric #-}
{-# language DeriveAnyClass #-}
{-# language TypeFamilies #-}

module CodeWorld.Tasks.Reify (
  ReifyPicture(..),
  Picture,
  share,
  toInterface
  ) where


import Control.DeepSeq                  (NFData)
import Data.Foldable                    (toList)
import Data.IntMap                      (IntMap, Key)
import Data.Reify                       (Graph(..), MuRef(..), reifyGraph)
import Data.Text                        (Text)
import GHC.Generics                     (Generic)
import qualified Data.IntMap            as IM

import CodeWorld.Tasks.API              (Drawable(..))
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


newtype Picture = PRec (ReifyPicture Picture) deriving (Show,Eq,Generic,NFData)


instance Semigroup Picture where
  (<>) = (&)


instance Monoid Picture where
  mempty  = blank
  mconcat = pictures


instance Drawable Picture where
  rectangle x          = PRec . Rectangle x
  thickRectangle t x   = PRec . ThickRectangle t x
  solidRectangle x     = PRec . SolidRectangle x
  circle               = PRec . Circle
  thickCircle t        = PRec . ThickCircle t
  solidCircle          = PRec . SolidCircle
  arc a1 a2            = PRec . Arc a1 a2
  sector a1 a2         = PRec . Sector a1 a2
  thickArc t a1 a2     = PRec . ThickArc t a1 a2
  curve                = PRec . Curve
  thickCurve t         = PRec . ThickCurve t
  closedCurve          = PRec . ClosedCurve
  thickClosedCurve t   = PRec . ThickClosedCurve t
  solidClosedCurve     = PRec . SolidClosedCurve
  polyline             = PRec . Polyline
  thickPolyline t      = PRec . ThickPolyline t
  polygon              = PRec . Polygon
  thickPolygon t       = PRec . ThickPolygon t
  solidPolygon         = PRec . SolidPolygon
  lettering            = PRec . Lettering
  styledLettering ts f = PRec . StyledLettering ts f
  colored c            = PRec . Color c
  translated x y       = PRec . Translate x y
  scaled x y           = PRec . Scale x y
  dilated a            = PRec . Dilate a
  rotated a            = PRec . Rotate a
  reflected a          = PRec . Reflect a
  clipped x y          = PRec . Clip x y
  pictures             = PRec . Pictures
  a & b                = PRec $ And a b
  coordinatePlane      = PRec CoordinatePlane
  codeWorldLogo        = PRec Logo
  blank                = PRec Blank


instance MuRef Picture where
  type DeRef Picture = ReifyPicture
  mapDeRef f (PRec body) = case body of
    Color c p       -> Color c <$> f p
    Translate x y p -> Translate x y <$> f p
    Scale x y p     -> Scale x y <$> f p
    Dilate x p      -> Dilate x <$> f p
    Rotate a p      -> Rotate a <$> f p
    Reflect a p     -> Reflect a <$> f p
    Clip x y p      -> Clip x y <$> f p
    And a b         -> And <$> f a <*> f b
    Pictures ps     -> Pictures <$> traverse f ps
    _               -> pure $ changeBaseType body


changeBaseType :: ReifyPicture a1 -> ReifyPicture a2
changeBaseType p = case p of
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
  _                     -> error "This is a recursive Constructor. You're missing a pattern match!"


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


toInterface :: Drawable a => Picture -> a
toInterface (PRec p) = case p of
  Rectangle x y -> rectangle x y
  ThickRectangle t x y -> thickRectangle t x y
  SolidRectangle x y -> solidRectangle x y
  Circle r -> circle r
  ThickCircle t r -> thickCircle t r
  SolidCircle r -> solidCircle r
  Polygon ps -> polygon ps
  SolidPolygon ps -> solidPolygon ps
  ThickPolygon t ps -> thickPolygon t ps
  ClosedCurve ps -> closedCurve ps
  SolidClosedCurve ps -> solidClosedCurve ps
  ThickClosedCurve t ps -> thickClosedCurve t ps
  Polyline ps -> polyline ps
  ThickPolyline t ps -> thickPolyline t ps
  Curve ps -> curve ps
  ThickCurve t ps -> thickCurve t ps
  Sector a1 a2 r -> sector a1 a2 r
  Arc a1 a2 r -> arc a1 a2 r
  ThickArc t a1 a2 r -> thickArc t a1 a2 r
  Lettering t -> lettering t
  StyledLettering ts f t -> styledLettering ts f t
  Color c q -> colored c $ toInterface q
  Translate x y q -> translated x y $ toInterface q
  Scale x y q -> scaled x y $ toInterface q
  Dilate fac q -> dilated fac $ toInterface q
  Rotate a q -> rotated a $ toInterface q
  Reflect a q -> reflected a $ toInterface q
  Clip x y q -> clipped x y $ toInterface q
  Pictures ps -> pictures $ map toInterface ps
  And p1 p2 -> toInterface p1 & toInterface p2
  CoordinatePlane -> coordinatePlane
  Logo -> codeWorldLogo
  Blank -> blank
