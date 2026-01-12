{-# Language OverloadedStrings #-}

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

rewriting (SolidCircle 0) = Blank

rewriting (Rectangle 0 _) = Blank
rewriting (Rectangle _ 0) = Blank
rewriting (Rectangle l w) = toWideRectangle Rectangle l w

rewriting (SolidRectangle 0 _) = Blank
rewriting (SolidRectangle _ 0) = Blank
rewriting (SolidRectangle l w) = toWideRectangle SolidRectangle l w

rewriting (ThickRectangle _ 0 _) = Blank
rewriting (ThickRectangle _ _ 0) = Blank
rewriting (ThickRectangle t l w)
  | t >= 2*l || t >= 2*w = SolidRectangle (l + t/2) (w + t/2)
  | otherwise = toWideRectangle (ThickRectangle t) l w

rewriting (Arc a1 a2 r) = checkArc Arc a1 a2 r
rewriting (ThickArc t a1 a2 r) = checkArc (ThickArc t) a1 a2 r
rewriting (Sector a1 a2 r) = checkArc Sector a1 a2 r

rewriting (Polygon ps) = Polyline $ toOpenShape ps
rewriting (ThickPolygon t ps) = ThickPolyline t $ toOpenShape ps
rewriting (ClosedCurve ps) = Curve $ toOpenShape ps
rewriting (ThickClosedCurve t ps) = ThickCurve t $ toOpenShape ps

rewriting (Lettering "") = Blank
rewriting (StyledLettering _ _ "") = Blank
rewriting (StyledLettering _ _ t) = Lettering t

rewriting (Translate 0 0 p) = p
rewriting (Translate x y p) = case p of
  Translate a b q -> Translate (x + a) (y + b) q
  Pictures ps     -> Pictures $ map (Translate x y) ps
  Blank           -> Blank
  Color c q       -> Color c $ Translate x y q
  _
    | isPointBased p -> applyToPoints p $ vectorSum (x,y)
    | otherwise      -> Translate x y p

rewriting (Color c p) = case p of
  Color _ q      -> Color c q
  Pictures ps    -> Pictures $ map (Color c) ps
  Blank          -> Blank
  _              -> if c == black then p else Color c p

rewriting (Dilate fac p) = Scale fac fac p

rewriting (Scale 0 _ _) = Blank
rewriting (Scale _ 0 _) = Blank
rewriting (Scale 1 1 p) = p
rewriting (Scale fac1 fac2 (Circle s)) | fac1 == fac2 =
  Circle (s * fac1)
rewriting (Scale fac1 fac2 (ThickCircle t s)) | fac1 == fac2 =
  ThickCircle t (s * fac1)
rewriting (Scale fac1 fac2 (SolidCircle s)) | fac1 == fac2 =
  SolidCircle (s * fac1)
rewriting (Scale fac1 fac2 (Rectangle s1 s2)) =
  Rectangle (s1 * fac1) (s2 * fac2)
rewriting (Scale fac1 fac2 (ThickRectangle t s1 s2)) =
  ThickRectangle t (s1 * fac1) (s2 * fac2)
rewriting (Scale fac1 fac2 (SolidRectangle s1 s2)) =
  SolidRectangle (s1 * fac1) (s2 * fac2)
rewriting (Scale fac1 fac2 p) = case p of
  Scale f1 f2 q    -> Scale (f1 * fac1) (f2 * fac2) q
  Translate x y q  -> Translate
    (x * fac1)
    (y * fac2)
    $ Scale fac1 fac2 q
  Blank            -> Blank
  Color c q        -> Color c $ Scale fac1 fac2 q
  Pictures ps      -> Pictures $ map (Scale fac1 fac2) ps
  _
    | isPointBased p -> applyToPoints p $ scaledVector fac1 fac2
    | otherwise      -> Scale fac1 fac2 p

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
      Rectangle x y
        | modAngle >= pi -> Rotate (modAngle - pi) $ Rectangle x y
      Circle r      -> Circle r
      _
        | isPointBased p -> applyToPoints p $ rotatedVector a
        | otherwise      -> Rotate a p
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
rewriting (Reflect a p)
  | isPointBased p = applyToPoints p $ reflectedPoint a
  | otherwise = Reflect a p

rewriting (And Blank p) = p
rewriting (And p Blank) = p
rewriting (And p q ) = if lowerPrecedence p q then And q p else And p q
rewriting p = p


-- Everything beyond here only considers the "open shape" variant if both kinds exist.
-- The closed variant is rewritten to an open one in a rule above.
lowerPrecedence :: Picture -> Picture -> Bool
lowerPrecedence (Polyline {}) p = isThickOrSolidPoly p || isCurve p
lowerPrecedence (ThickPolyline {}) (SolidPolygon {}) = True
lowerPrecedence (ThickPolyline {}) p = isCurve p
lowerPrecedence (Curve {}) p = isThickOrSolidCurve p
lowerPrecedence (ThickCurve {}) (SolidClosedCurve {}) = True
lowerPrecedence _ _ = False

isThickOrSolidPoly :: Picture -> Bool
isThickOrSolidPoly (ThickPolyline {}) = True
isThickOrSolidPoly (SolidPolygon {}) = True
isThickOrSolidPoly _ = False

isThickOrSolidCurve :: Picture -> Bool
isThickOrSolidCurve (ThickCurve {}) = True
isThickOrSolidCurve (SolidClosedCurve {}) = True
isThickOrSolidCurve _ = False

isPointBased :: Picture -> Bool
isPointBased (Polyline {}) = True
isPointBased p = isThickOrSolidPoly p || isCurve p

applyToPoints :: Picture -> (Point -> Point) -> Picture
applyToPoints (Polyline ps) f = Polyline $ map f ps
applyToPoints (ThickPolyline t ps) f = ThickPolyline t $ map f ps
applyToPoints (SolidPolygon ps) f = SolidPolygon $ map f ps
applyToPoints (Curve ps) f = Curve $ map f ps
applyToPoints (ThickCurve t ps) f = ThickCurve t $ map f ps
applyToPoints (SolidClosedCurve ps) f = SolidClosedCurve $ map f ps
applyToPoints p _ = p

isCurve :: Picture -> Bool
isCurve (Curve {}) = True
isCurve (ClosedCurve {}) = True
isCurve p = isThickOrSolidCurve p

toOpenShape :: [Point] -> [Point]
toOpenShape ps = ps ++ take 1 ps

toWideRectangle :: (Double -> Double -> Picture) -> Double -> Double -> Picture
toWideRectangle shape l w
    | l >= w = shape l w
    | otherwise = Rotate (pi/2) $ shape w l


-- angle still needs to be "mod 2pi'ed"
checkArc :: (Double -> Double -> Double -> Picture) -> Double -> Double -> Double -> Picture
checkArc _ _ _ 0 = Blank
checkArc shape a1 a2 r
  | a1 == a2  = Blank
  | a1 > a2 = shape a2 a1 r
  | abs (a1 - a2) >= 2*pi = circleKind r
  | otherwise = shape a1 a2 r
  where
    -- terrible hack
    circleKind = case shape undefined undefined undefined of
      Arc {}-> Circle
      ThickArc t _ _ _-> ThickCircle t
      Sector {}        -> SolidCircle
      _ -> error "That's not an arc!"
