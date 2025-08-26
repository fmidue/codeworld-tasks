module CodeWorld.Tasks.VectorSpace (
  -- CodeWorld interface
  translatedPoint,
  rotatedPoint,
  reflectedPoint,
  scaledPoint,
  dilatedPoint,
  vectorLength,
  vectorDirection,
  vectorSum,
  vectorDifference,
  scaledVector,
  rotatedVector,
  dotProduct,
  -- other stuff
  crossProduct,
  sideLengths,
  rotationAngle,
  isRectangle,
  atOriginWithOffset,
  wasTranslatedBy,
  wasScaledBy,
  wasRotatedBy
  ) where


import Data.Containers.ListUtils        (nubOrd)
import Data.List.Extra                  (headDef, takeEnd)
import Data.Maybe                       (fromMaybe)
import Data.Tuple.Extra                 (both)
import CodeWorld.Tasks.Types            (Point, Vector)



{-|
Moves a point in X and Y-directions.
-}
translatedPoint :: Double -> Double -> Point -> Point
translatedPoint x y (xp,yp) = (x+xp,y+yp)

{- |
Scales a point by given X and Y scaling factor.
Scaling by a negative factor also reflects across that axis.
-}
scaledPoint :: Double -> Double -> Point -> Point
scaledPoint = scaledVector


{-|
Dilates a point by given uniform scaling factor.
Dilating by a negative factor also reflects across the origin.
-}
dilatedPoint :: Double -> Point -> Point
dilatedPoint f = scaledPoint f f

{- |
Rotates a point around the origin by the given angle in radians.
-}
rotatedPoint :: Double -> Point -> Point
rotatedPoint = rotatedVector


getVector :: Point -> Point -> Vector
getVector (x1,y1) (x2,y2) = (x2-x1,y2-y1)


isOrthogonal :: Vector -> Vector -> Bool
isOrthogonal p = (==0) . dotProduct p


dotProduct :: Vector -> Vector -> Double
dotProduct (x1,y1) (x2,y2) = x1*x2 + y1*y2


crossProduct :: Vector -> Vector -> Double
crossProduct (a,b) (c,d) = a*d - c*b


{- |
The length of a vector.
-}
vectorLength :: Vector -> Double
vectorLength (x,y) = sqrt $ x*x + y*y


{- |
Scales a vector by the given scalar multiplier.
-}
scaledVector :: Double -> Double -> Vector -> Vector
scaledVector xFac yFac (x,y) = (x*xFac,y*yFac)

{- |
Rotates a vector by the given angle in radians.
-}
rotatedVector :: Double -> Vector -> Vector
rotatedVector angle (x,y) = (x * cos angle - y * sin angle, x * sin angle + y * cos angle)


{- |
The difference of two vectors.
-}
vectorDifference :: Vector -> Vector -> Vector
vectorDifference (x1,y1) (x2,y2) = (x1-x2,y1-y2)


{- |
The sum of two vectors.
-}
vectorSum :: Vector -> Vector -> Vector
vectorSum (x1,y1) (x2,y2) = (x1+x2,y1+y2)


{- |
The counter-clockwise angle of a vector from the X-axis.
-}
vectorDirection :: Vector -> Double
vectorDirection (x,y) = atan2 y x


{- |
Reflects a point across a line through the origin at this angle from the X-axis.
-}
reflectedPoint :: Double -> Point -> Point
reflectedPoint th (x, y) = (x * cos a + y * sin a, x * sin a - y * cos a)
  where a = 2 * th


allOrthogonal :: [Point] -> Bool
allOrthogonal (p1:p2:p3:xs) = isOrthogonal (getVector p1 p2) (getVector p2 p3) && allOrthogonal (p2:p3:xs)
allOrthogonal _ = True


sideLengths :: [Point] -> (Double,Double)
sideLengths (p@(xp,yp):ps) = (calc forX, calc forY)
  where
    forX = filter ((== yp) . snd) ps
    forY = filter ((== xp) . fst) ps
    calc val = vectorLength $ getVector (headDef p val) p
sideLengths _ = (0,0)


rectangleSideRotation :: [Point] -> Maybe Double
rectangleSideRotation (p1:p2:_) = angleToAxes (getVector p1 p2)
rectangleSideRotation _ = Nothing


angleToAxes :: Vector -> Maybe Double
angleToAxes v
  | dotProd == 0 = Nothing
  | angle == pi = Nothing
  | otherwise = Just angle
  where
    dotProd = dotProduct v (0,1)
    angle = acos $ dotProd / vectorLength v


rotationAngle :: [Point] -> Double
rotationAngle = fromMaybe 0 . rectangleSideRotation


tupleAbs :: (Ord a, Num a) => a -> (a, a) -> Bool
tupleAbs threshold (d1,d2) = abs d1 < threshold && abs d2 < threshold


{- |
Returns which translation needs to be applied to argument 1 to get argument 2.
Nothing if the polygons are different.

This can be used to detect translation in point list based shapes.
-}
wasTranslatedBy :: [Point] -> [Point] -> Maybe Point
wasTranslatedBy (p11:p12:ps1) (p21:p22:ps2)
  | tupleAbs eta (vectorDifference firstDiff (vectorDifference p22 p12)) &&
    length ps1 == length ps2 = Just firstDiff
  | otherwise = Nothing
  where
    firstDiff = vectorDifference p21 p11
wasTranslatedBy _ _ = Nothing


{- |
Returns which scaling factors need to be applied to argument 1 to get argument 2.

* Nothing if the polygons are not similar.
* Factors themselves can also be Nothing if the factor is undeterminable.

E.g. a possible result could be @Just (Just 3, Nothing)@.
This means the second shape is a scaled version of the first and the factor in X-direction is 3,
but the factor in Y-direction cannot be determined.

This can be used to detect size scaling in point list based shapes.
-}
wasScaledBy :: [Point] -> [Point] -> Maybe (Maybe Double,Maybe Double)
wasScaledBy ps1 ps2 | length ps1 == length ps2 =
  case both factor (matchX, matchY) of
    (Just a, Just b) -> Just (a,b)
    _                -> Nothing
  where
    (matchX, matchY) =
      let (x1s,y1s) = unzip ps1
          (x2s,y2s) = unzip ps2
      in  both (uncurry (zipWith (/))) ((x2s, x1s), (y2s, y1s))

    -- Nothing: Not a scaled version of the other list
    -- Just Nothing: is a scaled Version of the other list, but factor can't be determined
    -- Just fac: is a scaled version of the other list with factor fac
    handleResult [] = Just Nothing
    handleResult (ref:xs) | all (\x -> abs (ref - x) < eta) xs = Just $ Just ref
    handleResult _ = Nothing

    factor = handleResult . filter (not . isNaN)

wasScaledBy _ _ = Nothing


{- |
Returns which rotation needs to be applied to argument 1 to get argument 2.
Nothing if it does not exist.

This can be used to detect translation in point list based shapes.
-}
wasRotatedBy :: [Point] -> [Point] -> Maybe Double
wasRotatedBy ((x,y):(x2,y2):ps1) ((rx,ry):(rx2,ry2):ps2)
  | abs (firstRotation - atan2 (x2*ry2-y2*rx2) (x2*rx2+y2*ry2)) < eta &&
    length ps1 == length ps2 = Just firstRotation
  | otherwise = Nothing
  where
    firstRotation = atan2 (x*ry-y*rx) (x*rx+y*ry)
wasRotatedBy _ _ = Nothing


-- allowed difference to be considered "equal".
eta :: Double
eta = 0.0001


mean :: [Point] -> Point
mean [] = (0,0)
mean ps = scaledVector f f vSum
  where
    unique = nubOrd ps
    vSum = foldr vectorSum (0,0) unique
    f = 1/fromIntegral (length unique)


isRectangle :: [Point] -> Bool
isRectangle ps
    | hasFour && endIsStart = allOrthogonal unique
    | otherwise = False
  where
    unique = nubOrd ps
    hasFour = length unique == 4
    endIsStart = take 1 ps == takeEnd 1 ps


atOriginWithOffset :: [Point] -> ([Point],Point)
atOriginWithOffset [] = ([],(0,0))
atOriginWithOffset ps = (map (`vectorDifference` middlePoint) ps, middlePoint)
  where
    middlePoint = mean ps
