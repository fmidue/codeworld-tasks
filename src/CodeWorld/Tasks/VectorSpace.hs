module CodeWorld.Tasks.VectorSpace (
  addVectors,
  scaleVector,
  scaleVector2,
  rotateVector,
  reflectPoint,
  sideLengths,
  rotationAngle,
  isRectangle,
  atOriginWithOffset,
)
where


import Data.Containers.ListUtils        (nubOrd)
import Data.List.Extra                  (takeEnd)
import Data.Maybe                       (fromMaybe)
import CodeWorld.Tasks.Types            (Point, Vector)



getVector :: Point -> Point -> Vector
getVector (x1,y1) (x2,y2) = (x2-x1,y2-y1)


isOrthogonal :: Vector -> Vector -> Bool
isOrthogonal p = (==0) . dotProduct p


dotProduct :: Vector -> Vector -> Double
dotProduct (x1,y1) (x2,y2) = x1*x2 + y1*y2


vectorLen :: Vector -> Double
vectorLen (x,y) = sqrt $ x*x + y*y


scaleVector :: Double -> Vector -> Vector
scaleVector fac (x,y) = (x*fac,y*fac)


scaleVector2 :: Double -> Double -> Vector -> Vector
scaleVector2 xFac yFac (x,y) = (x*xFac,y*yFac)


rotateVector :: Double -> Vector -> Vector
rotateVector angle (x,y) = (x * cos angle - y * sin angle, x * sin angle + y * cos angle)


subVectors :: Vector -> Vector -> Vector
subVectors (x1,y1) (x2,y2) = (x1-x2,y1-y2)


addVectors :: Vector -> Vector -> Vector
addVectors (x1,y1) (x2,y2) = (x1+x2,y1+y2)


reflectPoint :: Double -> Point -> Point
reflectPoint th (x, y) = (x * cos a + y * sin a, x * sin a - y * cos a)
  where a = 2 * th


allOrthogonal :: [Point] -> Bool
allOrthogonal (p1:p2:p3:xs) = isOrthogonal (getVector p1 p2) (getVector p2 p3) && allOrthogonal (p2:p3:xs)
allOrthogonal _ = True


sideLengths :: [Point] -> (Double,Double)
sideLengths (p1:p2:p3:_) = (vectorLen (getVector p1 p2), vectorLen(getVector p2 p3))
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
    angle = acos $ dotProd / vectorLen v


rotationAngle :: [Point] -> Double
rotationAngle ps = fromMaybe 0 $ wasRotatedBy ps


mean :: [Point] -> Point
mean [] = (0,0)
mean ps = scaleVector (1/fromIntegral (length unique)) vSum
  where
    unique = nubOrd ps
    vSum = foldr addVectors (0,0) unique


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
atOriginWithOffset ps = (map (`subVectors` middlePoint) ps, middlePoint)
  where
    middlePoint = mean ps
