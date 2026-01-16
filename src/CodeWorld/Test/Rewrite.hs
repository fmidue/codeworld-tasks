{-# Language OverloadedStrings #-}
{-# Language ViewPatterns #-}

module CodeWorld.Test.Rewrite (
  normalize,
  normalizeNoOrder,
  normalizeAndAbstract,
  ) where


import Data.Fixed                       (mod')
import Data.Generics.Uniplate.Data      (rewrite)
import Data.List.Extra                  (sort, takeEnd)

import CodeWorld.Tasks.Color            (black)
import CodeWorld.Tasks.VectorSpace (
  Point,
  atOriginWithOffset,
  isRectangle,
  reflectedPoint,
  rotatedVector,
  rotationAngle,
  scaledVector,
  sideLengths,
  vectorSum,
  )
import CodeWorld.Tasks.Picture          (Picture(..), toInterface)
import CodeWorld.Tasks.Types            (Shape(..), Style(..))
import CodeWorld.Test.Abstract          (AbstractPicture)



{- |
Apply a set of rewriting rules to the Picture's syntax tree,
then abstract concrete parameters of the nodes,
resulting in an t`CodeWorld.Test.AbstractPicture`.

The new tree is normalized, simplified
and allows for more /fuzzy/ comparisons and queries.
-}
normalizeAndAbstract :: Picture -> AbstractPicture
normalizeAndAbstract = toInterface . normalize

{- |
Apply a set of rewriting rules to the Picture's syntax tree.
The result is a normalized and simplified tree in /canonical/ form,
which draws the same image.
-}
normalize :: Picture -> Picture
normalize = rewrite applyRewritingRules

{- |
Same as `normalize`,
but also erases information on which subpictures are drawn in front or behind others.
-}
normalizeNoOrder :: Picture -> Picture
normalizeNoOrder p = case normalize p of
  Pictures ps -> Pictures $ sort ps
  rp          -> rp


applyRewritingRules :: Picture -> Maybe Picture
applyRewritingRules p
  | p == rewritten = Nothing
  | otherwise = Just rewritten
  where
    rewritten = rewriting p

rewriting :: Picture -> Picture
rewriting (AnyCircle _ 0) = Blank
rewriting (ThickCircle t (abs -> r))
  | t == 2 * r = SolidCircle (r + t/2)
rewriting (AnyCircle s (abs -> r)) = AnyCircle s r

rewriting (AnyRectangle _ 0 _) = Blank
rewriting (AnyRectangle _ _ 0) = Blank
rewriting (ThickRectangle t (abs -> l) (abs -> w))
  | t >= 2*l || t >= 2*w = SolidRectangle (l + t/2) (w + t/2)
rewriting (AnyRectangle s (abs -> l) (abs -> w)) = toWideRectangle s l w

rewriting (AnyArc s a1 a2 r) = checkArc s a1 a2 r

rewriting (AnyPolyline (Closed (Outline mOutline)) ps) = AnyPolyline (Open mOutline) $ toOpenShape ps
rewriting (AnyPolyline s ps) = checkForRectangle s ps

rewriting (AnyCurve (Closed (Outline mOutline)) ps) = AnyCurve (Open mOutline) $ toOpenShape ps
rewriting (AnyCurve s ps) = handlePointList (AnyCurve s) ps

rewriting (Lettering "") = Blank
rewriting (StyledLettering _ _ "") = Blank
rewriting (StyledLettering _ _ t) = Lettering t

rewriting (Translate 0 0 p) = p
rewriting (Translate x y p) = case p of
  Translate a b q  -> Translate (x + a) (y + b) q
  Pictures ps      -> Pictures $ map (Translate x y) ps
  Blank            -> Blank
  Color c q        -> Color c $ Translate x y q
  AnyPolyline s ps -> AnyPolyline s $ map (vectorSum (x,y)) ps
  AnyCurve s ps    -> AnyCurve s $ map (vectorSum (x,y)) ps
  _                -> Translate x y p

rewriting (Color c p) = case p of
  Color _ q      -> Color c q
  Pictures ps    -> Pictures $ map (Color c) ps
  Blank          -> Blank
  _              -> if c == black then p else Color c p

rewriting (Dilate d p) = Scale d d p

rewriting (Scale 0 _ _) = Blank
rewriting (Scale _ 0 _) = Blank
rewriting (Scale 1 1 p) = p
rewriting (Scale fac1 fac2 (AnyCircle s r))
  | fac1 == fac2 = AnyCircle s (r * fac1)
rewriting (Scale fac1 fac2 (AnyRectangle s l w)) =
  AnyRectangle s (l * fac1) (w * fac2)
rewriting (Scale fac1 fac2 p) = case p of
  Scale f1 f2 q    -> Scale (f1 * fac1) (f2 * fac2) q
  Translate x y q  -> Translate
    (x * fac1)
    (y * fac2)
    $ Scale fac1 fac2 q
  Blank            -> Blank
  Color c q        -> Color c $ Scale fac1 fac2 q
  Pictures ps      -> Pictures $ map (Scale fac1 fac2) ps
  AnyPolyline s ps -> AnyPolyline s $ map (scaledVector fac1 fac2) ps
  AnyCurve s ps    -> AnyCurve s $ map (scaledVector fac1 fac2) ps
  _                -> Scale fac1 fac2 p

rewriting (Rotate (capAngle -> a) p)
    | a == 0 = p
    | otherwise = case p of
      Scale fac1 fac2 c@(AnyCircle {})
        | a == pi/2 || a == 3*pi/2
                      -> Scale fac2 fac1 c
      Rotate a2 q     -> Rotate (a + a2) q
      Reflect a2 q    -> Reflect (a2 + a/2) q
      Translate x y q -> Translate
                          (x*cos a - y*sin a)
                          (x*sin a + y*cos a)
                          $ Rotate a q
      Color c q       -> Color c $ Rotate a q
      Pictures ps     -> Pictures $ map (Rotate a) ps
      r@AnyRectangle {}
        | a >= pi -> Rotate (a - pi) r
      c@AnyCircle {}      -> c
      AnyPolyline s ps -> AnyPolyline s $ map (rotatedVector a) ps
      AnyCurve s ps    -> AnyCurve s $ map (rotatedVector a) ps
      _                -> Rotate a p

rewriting (Reflect (capAngle -> a1) (Reflect (capAngle -> a2) p))
  | a1 == a2 = p
  | otherwise = Rotate (capAngle $ a1*2 - a2*2) p
rewriting (Reflect a r@(AnyRectangle {})) = Rotate (capAngle $ a*2) r
rewriting (Reflect _ c@(AnyCircle {})) = c
rewriting (Reflect a (Pictures ps)) = Pictures $ map (Reflect a) ps
rewriting (Reflect (capAngle -> a) (Translate x y p)) =
  let
    twoTimesSquaredSubOne f = 2 * f a^(2 :: Int) -1
    twoTimesCosSin = 2 * cos a * sin a
  in Translate
    (twoTimesSquaredSubOne cos * x + twoTimesCosSin * y)
    (twoTimesCosSin * x + twoTimesSquaredSubOne sin * y)
    $ Reflect a p
rewriting (Reflect a (Rotate a2 p)) = Reflect (capAngle $ a - (a2/2)) p
rewriting (Reflect a (Color c q))   = Color c $ Reflect a q
rewriting (Reflect (capAngle -> a) p) = case p of
  AnyPolyline s ps -> AnyPolyline s $ map (reflectedPoint a) ps
  AnyCurve s ps    -> AnyCurve s $ map (reflectedPoint a) ps
  _                -> Reflect a p

rewriting (Pictures ps) = foldr (\a -> rewriting . And a) Blank ps
rewriting (And Blank p) = p
rewriting (And p Blank) = p
rewriting (And (AnyPolyline s1 ps1) (AnyPolyline s2 ps2))
  | s1 == s2 = handleLikeFreeShapes (AnyPolyline s1) ps1 ps2
rewriting (And (AnyCurve s1 ps1) (AnyCurve s2 ps2))
  | s1 == s2 = handleLikeFreeShapes (AnyCurve s1) ps1 ps2
rewriting (And p q) = if lowerPrecedence p q
    then And q p
    else Pictures $  ps1 ++ ps2
  where
    ps1 = case p of
      Pictures ps -> ps
      _           -> [p]
    ps2 = case q of
      Pictures ps -> ps
      _           -> [q]
rewriting p = p


-- This only considers the "open shape" variant if both kinds exist.
-- The closed variant is rewritten to an open one in a rule above.
lowerPrecedence :: Picture -> Picture -> Bool
lowerPrecedence (AnyPolyline {}) (AnyCurve {}) = True
lowerPrecedence (AnyPolyline (Open _) _) (SolidPolygon {}) = True
lowerPrecedence (Polyline {}) (ThickPolyline {}) = True
lowerPrecedence (AnyCurve (Open _) _) (SolidClosedCurve {}) = True
lowerPrecedence (Curve {}) (ThickCurve {}) = True
lowerPrecedence _ _ = False

toOpenShape :: [Point] -> [Point]
toOpenShape ps = ps ++ take 1 ps

toWideRectangle :: Style -> Double -> Double -> Picture
toWideRectangle style l w
    | l >= w = AnyRectangle style l w
    | otherwise = Rotate (pi/2) $ AnyRectangle style w l


checkArc :: Style -> Double -> Double -> Double -> Picture
checkArc _ _ _ 0 = Blank
checkArc style (capAngle -> a1) (capAngle -> a2) r
  | a1 == a2              = Blank
  | a1 > a2               = AnyArc style a2 a1 r
  | abs (a1 - a2) >= 2*pi = AnyCircle style r
  | otherwise             = AnyArc style a1 a2 r

handlePointList :: ([Point] -> Picture) -> [Point] -> Picture
handlePointList shape ps
    | length noRepeats < 2 = Blank
    | otherwise = shape noRepeats
  where
    noRepeats = removeDupes ps

removeDupes :: Eq a => [a] -> [a]
removeDupes (x:y:xs)
  | x == y    =      rec
  | otherwise =  x : rec
  where rec = removeDupes (y:xs)
removeDupes xs = xs

handleLikeFreeShapes
  :: ([Point] -> Picture)
  -> [Point]
  -> [Point]
  -> Picture
handleLikeFreeShapes s1 ps1 ps2
  | endPs1 == startPs2
  = s1 $ ps1 ++ restPs2
  | endPs1 == startRevPs2
  = s1 $ ps1 ++ endRevPs2
  | otherwise = Pictures [s1 ps1, s1 ps2]
    where
      (startPs2,restPs2) = splitAt 1 ps2
      (startRevPs2, endRevPs2) = splitAt 1 $ reverse ps2
      endPs1 = takeEnd 1 ps1

checkForRectangle :: Shape -> [Point] -> Picture
checkForRectangle shape ps = case pointsToRectangle shape ps of
  Nothing -> handlePointList (AnyPolyline shape) ps
  Just r  -> r

pointsToRectangle :: Shape -> [Point] -> Maybe Picture
pointsToRectangle shapeKind ps
  | isRectangle ps = Just $ Translate x y $ Rotate angle $ shapeToUse xLen yLen
  | otherwise = Nothing
  where
    (xLen,yLen) = sideLengths ps
    angle = rotationAngle originPs
    (originPs,(x,y)) = atOriginWithOffset (drop 1 ps)
    shapeToUse = case shapeKind of
      Closed (Outline (Just t)) -> ThickRectangle t
      Open (Just t)             -> ThickRectangle t
      Closed Solid              -> SolidRectangle
      _                         -> Rectangle

capAngle :: (Floating a, Real a) => a -> a
capAngle a = a `mod'` (2*pi)
