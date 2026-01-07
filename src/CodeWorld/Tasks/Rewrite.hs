module CodeWorld.Tasks.Rewrite (
  rewriting,
  maybeRewritten,
  ) where

import CodeWorld.Test


maybeRewritten :: Picture -> Maybe Picture
maybeRewritten p
  | p == rewritten = Nothing
  | otherwise = Just rewritten
  where
    rewritten = rewriting p

rewriting :: Picture -> Picture
rewriting (Circle 0) = Blank
rewriting (ThickCircle 0 _) = Blank
rewriting (ThickCircle t r)
  | t == 2 * r = SolidCircle (r + t/2)

rewriting (Rectangle 0 _) = Blank
rewriting (Rectangle _ 0) = Blank

rewriting (SolidRectangle 0 _) = Blank
rewriting (SolidRectangle _ 0) = Blank

rewriting (ThickRectangle _ 0 _) = Blank
rewriting (ThickRectangle _ _ 0) = Blank
rewriting (ThickRectangle t l w)
  | t >= 2*l || t >= 2*w = SolidRectangle (l + t/2) (w + t/2)

rewriting (Translate 0 0 p) = p
rewriting (Translate x y p) = case p of
  Translate a b q -> Translate (x + a) (y + b) q
  Pictures ps     -> Pictures $ map (Translate x y) ps
  Blank           -> Blank
  Color c q       -> Color c $ Translate x y q
  -- other polyshapes!
  Polyline ps     -> Polyline $ map (vectorSum (x,y)) ps
  Curve ps        -> Curve $ map (vectorSum (x,y)) ps
  a               -> Translate x y a
rewriting (Color c p) = case p of
  Color _ q      -> Color c q
  Pictures ps    -> Pictures $ map (Color c) ps
  Blank          -> Blank
  q              -> if c == black then q else Color c q
rewriting (Dilate fac p) = Scale fac fac p

rewriting (Scale 0 _ _) = Blank
rewriting (Scale _ 0 _) = Blank
rewriting (Scale 1 1 p) = p
-- other circles missing!
rewriting (Scale fac1 fac2 (Circle s)) | fac1 == fac2 =
  Circle (s * fac1)
-- other rectangles missing!
rewriting (Scale fac1 fac2 (Rectangle s1 s2)) =
  Rectangle (s1 * fac1) (s2 * fac2)
rewriting (Scale fac1 fac2 p) = case p of
  Scale f1 f2 q    -> Scale (f1 * fac1) (f2 * fac2) q
  Translate x y q  -> Translate
    (x * fac1)
    (y * fac2)
    $ Scale fac1 fac2 q
  Blank            -> Blank
  Color c q        -> Color c $ Scale fac1 fac2 q
  Pictures ps      -> Pictures $ map (Scale fac1 fac2) ps
  -- other polyshapes!
  Polyline ps    -> Polyline $ map (scaledVector fac1 fac2) ps
  Curve ps       -> Curve $ map (scaledVector fac1 fac2) ps
  a                -> Scale fac1 fac2 a

-- Seems I need to do more mod 2*pi here now...
rewriting (Rotate a p)
    | modAngle == 0 = p
    | otherwise = case p of
      Scale fac1 fac2 c@(Circle {})
        | modAngle == pi/2 || modAngle == 3*pi/2
                      -> Scale fac2 fac1 c
      Rotate a2 q     -> Rotate (a + a2) q
      Reflect a2 q    -> Reflect (a2 + a/2) q
      Translate x y q -> Translate
                          (x*cos a - y*sin a)
                          (x*sin a + y*cos a)
                          $ Rotate a q
      Color c q       -> Color c $ Rotate a q
      Pictures ps     -> Pictures $ map (Rotate a) ps
      -- other shapes!
      Polyline ps   -> Polyline $ map (rotatedVector a) ps
      Curve ps      -> Curve $ map (rotatedVector a) ps
      Rectangle x y
        | modAngle >= pi -> Rotate (modAngle - pi) $ Rectangle x y
      Circle r      -> Circle r
      q               -> Rotate a q
    where
      modAngle = toAngle a
      toAngle ang
        | ang < 0 = toAngle (ang+2*pi)
        | ang < 2*pi = ang
        | otherwise = toAngle (ang-2*pi)

-- shapes and angle mods missing here...
rewriting (Reflect a1 (Reflect a2 p))
  | a1 == a2 = p
  | otherwise = Rotate (a1*2 - a2*2) p
rewriting (Reflect a (Rectangle x y)) = Rotate (a*2) $ Rectangle x y
rewriting (Reflect _ (Circle r)) = Circle r
rewriting (Reflect a (Polyline ps)) = Polyline $ map (reflectedPoint a) ps
rewriting (Reflect a (Curve ps)) = Curve $ map (reflectedPoint a) ps
rewriting (Reflect a (Pictures ps)) = Pictures $ map (Reflect a) ps
rewriting (Reflect a (Translate x y p)) =
  let
    twoTimesSquaredSubOne f = 2 * f a^(2 :: Int) -1
    twoTimesCosSin = 2 * cos a * sin a
  in Translate
    (twoTimesSquaredSubOne cos * x + twoTimesCosSin * y)
    (twoTimesCosSin * x + twoTimesSquaredSubOne sin * y)
    $ Reflect a p
rewriting (Reflect a (Rotate a2 p)) = Reflect (a - (a2/2)) p
rewriting (Reflect a (Color c q))   = Color c $ Reflect a q
rewriting (Reflect a p) = Reflect a p

rewriting (And Blank p) = p
rewriting (And p Blank) = p
rewriting (And p q ) = if lowerPrecedence p q then And q p else And p q
rewriting p = p


lowerPrecedence :: Picture -> Picture -> Bool
lowerPrecedence (Polyline {}) p = isThickOrSolidPoly p || isCurve p
lowerPrecedence (ThickPolygon {}) (SolidPolygon {}) = True
lowerPrecedence (ThickPolygon {}) p = isCurve p
lowerPrecedence (ThickPolyline {}) (SolidPolygon {}) = True
lowerPrecedence (ThickPolyline {}) p = isCurve p
lowerPrecedence (Polygon {}) p = isThickOrSolidPoly p || isCurve p
lowerPrecedence (Curve {}) p = isThickOrSolidCurve p
lowerPrecedence (ClosedCurve {}) p = isThickOrSolidCurve p
lowerPrecedence (ThickCurve {}) (SolidClosedCurve {}) = True
lowerPrecedence (ThickClosedCurve {}) (SolidClosedCurve {}) = True
lowerPrecedence _ _ = False

isThickOrSolidPoly :: Picture -> Bool
isThickOrSolidPoly (ThickPolyline {}) = True
isThickOrSolidPoly (ThickPolygon {}) = True
isThickOrSolidPoly (SolidPolygon {}) = True
isThickOrSolidPoly _ = False

isThickOrSolidCurve :: Picture -> Bool
isThickOrSolidCurve (ThickCurve {}) = True
isThickOrSolidCurve (ThickClosedCurve {}) = True
isThickOrSolidCurve (SolidClosedCurve {}) = True
isThickOrSolidCurve _ = False

isCurve :: Picture -> Bool
isCurve (Curve {}) = True
isCurve (ClosedCurve {}) = True
isCurve p = isThickOrSolidCurve p
