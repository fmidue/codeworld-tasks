{-# LANGUAGE FlexibleContexts #-}

module CodeWorld.Test.Solution (
  PicPredicate,
  complain,
  testPicture,
  testAnimation,
  containsElem,
  containsElems,
  containsExactElems,
  hasRelation,
  (<||>),
  (<&&>),
  (<^^>),
  option,
  options,
  ifThen,
  thisOften,
  atLeast,
  atMost,
  inRangeOf,
  rawImage,
  normalizedImage,
  findMaybe,
  findAll,
  findAllAnd,
  findMaybeAnd,
  findAllActual,
  findMaybeActual,
  findAllActualAnd,
  findMaybeActualAnd,
  oneOf,
  mapAnimation,
  atTime,
  rawImagesAt,
  normalizedImagesAt,
  allAt,
  allAtWithTime,
  anyAt,
  noneAt,
  queryAt,
  ) where

import Control.Monad (unless)
import Control.Monad.Except
import Control.Monad.Reader
import Data.Either (fromLeft)
import Data.Maybe (listToMaybe)
import Data.Traversable (for)

import CodeWorld.Tasks.Picture (Picture)
import CodeWorld.Test.Abstract (
  AbstractPicture(..),
  contains,
  count,
  getSubPictures,
  stripTranslation,
  )
import CodeWorld.Test.Relative (
  Components(..),
  SpatialQuery,
  toRelative,
  )
import CodeWorld.Test.Rewrite (
  normalize,
  normalizeAndAbstract
  )



{- |
Alias for predicates on `Components`.
-}
type PicPredicate = Reader (Picture,Components) Bool


{- |
At least one of many predicates evaluates to True.
-}
options :: MonadReader env m => [m Bool] -> m Bool
options = foldr (<||>) (pure False)


{- |
At least one of two predicates evaluates to True.
-}
(<||>) :: MonadReader env m => m Bool -> m Bool -> m Bool
(<||>) p q = (||) <$> p <*> q


{- |
Both predicates evaluate to True.
-}
(<&&>) :: MonadReader env m => m Bool -> m Bool -> m Bool
(<&&>) p q = (&&) <$> p <*> q


{- |
Only one of two predicates evaluate to True (XOR).
-}
(<^^>) :: MonadReader env m => m Bool -> m Bool -> m Bool
(<^^>) a b = not <$> a <&&> b


{- |
Alias for (`<||>`)
-}
option :: MonadReader env m => m Bool -> m Bool -> m Bool
option = (<||>)


{- |
The predicate is satisfied by at least one of the given options.
Use when there's multiple shape primitives a student could use to solve the task.
-}
oneOf :: (a -> PicPredicate) -> [a] ->  PicPredicate
oneOf p = fmap or . mapM p


-- Apply a function to the list of sub images and retrieve the result
specElems :: (AbstractPicture -> a) -> (Components -> a)
specElems f (Components (ps,_)) = f ps


{- |
Returns the first picture element satisfying the predicate if it exists. (translation is removed)
-}
findMaybe
  :: MonadReader (Picture,Components) m
  => (AbstractPicture -> Bool)
  -> m (Maybe AbstractPicture)
findMaybe = fmap listToMaybe . findAll


{- |
Returns all picture elements satisfying the predicate. (translation is removed)
-}
findAll
  :: MonadReader (Picture,Components) m
  => (AbstractPicture -> Bool)
  -> m [AbstractPicture]
findAll f = asks $ filter f . specElems (map stripTranslation . getSubPictures) . snd


{- |
Returns all subpictures satisfying the predicate. (includes translation)
-}
findAllActual
  :: MonadReader (Picture,Components) m
  => (AbstractPicture -> Bool)
  -> m [AbstractPicture]
findAllActual f = asks $ filter f . specElems getSubPictures . snd


{- |
Returns the first subpicture satisfying the predicate if it exists. (includes translation)
-}
findMaybeActual
  :: MonadReader (Picture,Components) m
  => (AbstractPicture -> Bool)
  -> m (Maybe AbstractPicture)
findMaybeActual = fmap listToMaybe . findAllActual


{- |
Finds all subpictures satisfying a predicate, then applies a function. (includes translation)
-}
findAllActualAnd
  :: MonadReader (Picture,Components) m
  => (AbstractPicture -> Bool)
  -> (AbstractPicture -> a)
  -> m [a]
findAllActualAnd p f = map f <$> findAllActual p


{- |
Finds the first subpicture satisfying a predicate, then applies a function if it exists. (includes translation)
-}
findMaybeActualAnd
  :: MonadReader (Picture,Components) m
  => (AbstractPicture -> Bool)
  -> (AbstractPicture -> a)
  -> m (Maybe a)
findMaybeActualAnd p = fmap listToMaybe . findAllActualAnd p


{- |
Finds all picture elements satisfying a predicate, then applies a function. (translation is removed)
-}
findAllAnd
  :: MonadReader (Picture,Components) m
  => (AbstractPicture -> Bool)
  -> (AbstractPicture -> a)
  -> m [a]
findAllAnd f g = map g <$> findAll f


{- |
Finds the first element satisfying a predicate, then applies a function if it exists. (translation is removed)
-}
findMaybeAnd
  :: MonadReader (Picture,Components) m
  => (AbstractPicture -> Bool)
  -> (AbstractPicture -> a)
  -> m (Maybe a)
findMaybeAnd f = fmap listToMaybe . findAllAnd f


{- |
Returns the unmodified Picture.
-}
rawImage :: MonadReader (Picture,Components) m => m Picture
rawImage = asks fst


{- |
Returns the normalized Picture.
-}
normalizedImage :: MonadReader (Picture,Components) m => m Picture
normalizedImage = asks (normalize . fst)


{- |
True if image contains exactly these subpictures and nothing else.
-}
containsExactElems :: [AbstractPicture] -> PicPredicate
containsExactElems ps = asks $ specElems
  (all (\tp -> Pictures ps `contains` tp) . getSubPictures) . snd


{- |
True if image contains at least this subpicture and optionally something else.
-}
containsElem :: AbstractPicture -> PicPredicate
containsElem p = asks $ specElems (`contains` p) . snd

{- |
True if image contains at least these subpictures and optionally something else.
-}
containsElems :: [AbstractPicture] -> PicPredicate
containsElems ps = asks $ specElems (\t -> all (\p -> t `contains` p) ps) . snd


{- |
True if image contains this subpicture exactly this many times.
-}
thisOften :: AbstractPicture -> Int -> PicPredicate
thisOften p amount = asks $ specElems (\ps -> count p ps == amount) . snd


{- |
True if image contains this subpicture at least this many times.
-}
atLeast :: AbstractPicture -> Int -> PicPredicate
atLeast p amount = asks $ specElems (\ps -> count p ps >= amount) . snd


{- |
True if image contains this subpicture at most many times.
-}
atMost :: AbstractPicture -> Int -> PicPredicate
atMost p amount = asks $ specElems (\ps -> count p ps <= amount) . snd


{- |
True if amount of times this subpicture is contained in the image lies in the specified range.
-}
inRangeOf :: AbstractPicture -> (Int,Int) -> PicPredicate
inRangeOf p (lower,upper) = asks $
  specElems (\ps -> let occurs = count p ps in occurs >= lower && occurs <= upper) . snd


{- |
Runs the first predicate p, then the second q if p evaluated to `True`.
Does not run q if p evaluates to `False`.
-}
ifThen :: PicPredicate -> PicPredicate -> PicPredicate
ifThen p q = (not <$> p) <||> q


-- Use a predicate on the list of relative positions
specPosition :: SpatialQuery -> Components -> Bool
specPosition f (Components (_,rP)) = f rP


{- |
Transforms student submission into spatial `Components` form.
-}
getComponents :: Picture -> Components
getComponents = toRelative . normalizeAndAbstract


{- |
True if image contains the specified spatial relations.
Used with corresponding functions like `CodeWorld.Test.isNorthOf`, `CodeWorld.Test.isLeftOf`, etc.
-}
hasRelation :: SpatialQuery -> PicPredicate
hasRelation q = asks $ specPosition q . snd


{- |
Returns the animation environment with its output mapped over by the argument.
-}
mapAnimation
  :: Monad m
  => ((Picture,Components) -> a)
  -> ReaderT (Double -> (Picture,Components)) m (Double -> a)
mapAnimation = asks . fmap


{- |
Samples the animation at the given time point and applies a predicate or query to it.
-}
atTime :: Double -> ReaderT (Picture,Components) m a -> ReaderT (Double -> (Picture,Components)) m a
atTime time = withReaderT ($ time)


{- |
Samples the animation at multiple time points and applies a predicate or query to each image,
then returns a list of results.
-}
queryAt
  :: Applicative m
  => [Double]
  -> ReaderT (Picture,Components) m a
  -> ReaderT (Double -> (Picture,Components)) m [a]
queryAt frames = for frames . flip atTime


{- |
Samples the animation at multiple time points
and applies a predicate dependent on the current time point to each image.
Returns 'True' if all samples satisfied the predicate.
-}
queryAtWithTime
  :: Applicative m
  => [Double]
  -> (Double -> ReaderT (Picture,Components) m a)
  -> ReaderT (Double -> (Picture,Components)) m [a]
queryAtWithTime frames action = for frames $ \t -> atTime t (action t)


{- |
Samples the animation at multiple time points and applies a predicate to each image.
Returns 'True' if all samples satisfied the predicate.
-}
allAt :: [Double] -> Reader (Picture,Components) Bool -> Reader (Double -> (Picture,Components)) Bool
allAt frames = fmap and . queryAt frames


{- |
Samples the animation at multiple time points
and applies a predicate dependent on the current time point to each image.
Returns 'True' if all samples satisfied the predicate.
-}
allAtWithTime
  :: [Double]
  -> (Double -> Reader (Picture,Components) Bool)
  -> Reader (Double -> (Picture,Components)) Bool
allAtWithTime frames = fmap and . queryAtWithTime frames


{- |
Samples the animation at multiple time points and applies a predicate to each image.
Returns 'True' if any sample satisfied the predicate.
-}
anyAt :: [Double] -> Reader (Picture,Components) Bool -> Reader (Double -> (Picture,Components)) Bool
anyAt frames = fmap or . queryAt frames


{- |
Samples the animation at multiple time points and applies a predicate to each image.
Returns 'True' if none of the samples satisfied the predicate.
-}
noneAt :: [Double] -> Reader (Picture,Components) Bool -> Reader (Double -> (Picture,Components)) Bool
noneAt frames = fmap not . anyAt frames



{- |
Samples the animation at multiple time points and returns a list of unmodified results.
-}
rawImagesAt :: Monad m => [Double] -> ReaderT (Double -> (Picture,Components)) m [Picture]
rawImagesAt frames = queryAt frames rawImage


{- |
Samples the animation at multiple time points and returns a list of normalized results.
-}
normalizedImagesAt :: Monad m => [Double] -> ReaderT (Double -> (Picture,Components)) m [Picture]
normalizedImagesAt frames = queryAt frames normalizedImage


{- |
Builds a fallible test given an error message and a predicate.
-}
complain :: String -> Reader a Bool -> ReaderT a (Except String) ()
complain label r = do
  c <- ask
  unless (runReader r c) $ throwError label


{- |
Executes the given test suite on a static image and returns the error message of the first failed test
or the empty String if all tests passed.
-}
testPicture :: Picture -> ReaderT (Picture, Components) (Except String) () -> String
testPicture p = fromLeft "" . runExcept . flip runReaderT (p, getComponents p)


{- |
Executes the given test suite on an animation and returns the error message of the first failed test
or the empty String if all tests passed.
-}
testAnimation :: (Double -> Picture) -> ReaderT (Double -> (Picture,Components)) (Except String) () -> String
testAnimation p = fromLeft "" . runExcept . flip runReaderT (\t -> (p t, getComponents $ p t))
