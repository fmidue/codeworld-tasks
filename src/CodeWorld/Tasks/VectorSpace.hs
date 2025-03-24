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
  sideLengths,
  rotationAngle,
  isRectangle,
  atOriginWithOffset,
  ) where


import Data.Containers.ListUtils        (nubOrd)
import Data.List.Extra                  (takeEnd)
import Data.Maybe                       (fromMaybe)
import CodeWorld.Tasks.Types            (Point, Vector)



translatedPoint :: Double -> Double -> Point -> Point
translatedPoint x y (xp,yp) = (x+xp,y+yp)


scaledPoint :: Double -> Double -> Point -> Point
scaledPoint = scaledVector


dilatedPoint :: Double -> Point -> Point
dilatedPoint f = scaledPoint f f


rotatedPoint :: Double -> Point -> Point
rotatedPoint = rotatedVector


getVector :: Point -> Point -> Vector
getVector (x1,y1) (x2,y2) = (x2-x1,y2-y1)


isOrthogonal :: Vector -> Vector -> Bool
isOrthogonal p = (==0) . dotProduct p


dotProduct :: Vector -> Vector -> Double
dotProduct (x1,y1) (x2,y2) = x1*x2 + y1*y2


vectorLength :: Vector -> Double
vectorLength (x,y) = sqrt $ x*x + y*y


scaledVector :: Double -> Double -> Vector -> Vector
scaledVector xFac yFac (x,y) = (x*xFac,y*yFac)


rotatedVector :: Double -> Vector -> Vector
rotatedVector angle (x,y) = (x * cos angle - y * sin angle, x * sin angle + y * cos angle)


vectorDifference :: Vector -> Vector -> Vector
vectorDifference (x1,y1) (x2,y2) = (x1-x2,y1-y2)


vectorSum :: Vector -> Vector -> Vector
vectorSum (x1,y1) (x2,y2) = (x1+x2,y1+y2)


vectorDirection :: Vector -> Double
vectorDirection (x,y) = atan2 y x


reflectedPoint :: Double -> Point -> Point
reflectedPoint th (x, y) = (x * cos a + y * sin a, x * sin a - y * cos a)
  where a = 2 * th


allOrthogonal :: [Point] -> Bool
allOrthogonal (p1:p2:p3:xs) = isOrthogonal (getVector p1 p2) (getVector p2 p3) && allOrthogonal (p2:p3:xs)
allOrthogonal _ = True


sideLengths :: [Point] -> (Double,Double)
sideLengths (p1:p2:p3:_) = (vectorLength (getVector p1 p2), vectorLength (getVector p2 p3))
sideLengths _ = (0,0)


wasRotatedBy :: [Point] -> Maybe Double
wasRotatedBy (p1:p2:_) = angleToAxes (getVector p1 p2)
wasRotatedBy _ = Nothing


angleToAxes :: Vector -> Maybe Double
angleToAxes v
  | dotProd == 0 = Nothing
  | angle == pi = Nothing
  | otherwise = Just angle
  where
    dotProd = dotProduct v (0,1)
    angle = acos $ dotProd / vectorLength v


rotationAngle :: [Point] -> Double
rotationAngle ps = fromMaybe 0 $ wasRotatedBy ps


mean :: [Point] -> Point
mean [] = (0,0)
mean ps = scaledVector fac fac vSum
  where
    unique = nubOrd ps
    vSum = foldr vectorSum (0,0) unique
    fac = 1/fromIntegral (length unique)


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
