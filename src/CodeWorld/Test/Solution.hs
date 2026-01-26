{-# LANGUAGE FlexibleContexts #-}

module CodeWorld.Test.Solution (
  StaticImage,
  Animation,
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
import Control.Monad.Except             (Except, runExcept, throwError)
import Control.Monad.Reader (
  MonadReader,
  Reader,
  ReaderT,
  ask,
  asks,
  runReader,
  runReaderT,
  withReaderT
  )
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
The environment for tests on still images.
-}
type StaticImage = (Picture, Components)


{- |
The environment for tests on animations.
-}
type Animation = Double -> StaticImage


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
oneOf :: MonadReader env m => (a -> m Bool) -> [a] ->  m Bool
oneOf p = fmap or . mapM p


-- Apply a function to the list of sub images and retrieve the result
specElems :: (AbstractPicture -> a) -> (Components -> a)
specElems f (Components (ps,_)) = f ps


{- |
Returns the first picture element satisfying the predicate if it exists. (translation is removed)
-}
findMaybe
  :: MonadReader StaticImage m
  => (AbstractPicture -> Bool)
  -> m (Maybe AbstractPicture)
findMaybe = fmap listToMaybe . findAll


{- |
Returns all picture elements satisfying the predicate. (translation is removed)
-}
findAll
  :: MonadReader StaticImage m
  => (AbstractPicture -> Bool)
  -> m [AbstractPicture]
findAll f = asks $ filter f . specElems (map stripTranslation . getSubPictures) . snd


{- |
Returns all subpictures satisfying the predicate. (includes translation)
-}
findAllActual
  :: MonadReader StaticImage m
  => (AbstractPicture -> Bool)
  -> m [AbstractPicture]
findAllActual f = asks $ filter f . specElems getSubPictures . snd


{- |
Returns the first subpicture satisfying the predicate if it exists. (includes translation)
-}
findMaybeActual
  :: MonadReader StaticImage m
  => (AbstractPicture -> Bool)
  -> m (Maybe AbstractPicture)
findMaybeActual = fmap listToMaybe . findAllActual


{- |
Finds all subpictures satisfying a predicate, then applies a function. (includes translation)
-}
findAllActualAnd
  :: MonadReader StaticImage m
  => (AbstractPicture -> Bool)
  -> (AbstractPicture -> a)
  -> m [a]
findAllActualAnd p f = map f <$> findAllActual p


{- |
Finds the first subpicture satisfying a predicate, then applies a function if it exists. (includes translation)
-}
findMaybeActualAnd
  :: MonadReader StaticImage m
  => (AbstractPicture -> Bool)
  -> (AbstractPicture -> a)
  -> m (Maybe a)
findMaybeActualAnd p = fmap listToMaybe . findAllActualAnd p


{- |
Finds all picture elements satisfying a predicate, then applies a function. (translation is removed)
-}
findAllAnd
  :: MonadReader StaticImage m
  => (AbstractPicture -> Bool)
  -> (AbstractPicture -> a)
  -> m [a]
findAllAnd f g = map g <$> findAll f


{- |
Finds the first element satisfying a predicate, then applies a function if it exists. (translation is removed)
-}
findMaybeAnd
  :: MonadReader StaticImage m
  => (AbstractPicture -> Bool)
  -> (AbstractPicture -> a)
  -> m (Maybe a)
findMaybeAnd f = fmap listToMaybe . findAllAnd f


{- |
Returns the unmodified Picture.
-}
rawImage :: MonadReader StaticImage m => m Picture
rawImage = asks fst


{- |
Returns the normalized Picture.
-}
normalizedImage :: MonadReader StaticImage m => m Picture
normalizedImage = asks (normalize . fst)


{- |
True if image contains exactly these subpictures and nothing else.
-}
containsExactElems
  :: MonadReader StaticImage m
  => [AbstractPicture]
  -> m Bool
containsExactElems ps = asks $ specElems
  (all (\tp -> Pictures ps `contains` tp) . getSubPictures) . snd


{- |
True if image contains at least this subpicture and optionally something else.
-}
containsElem :: MonadReader StaticImage m => AbstractPicture -> m Bool
containsElem p = asks $ specElems (`contains` p) . snd

{- |
True if image contains at least these subpictures and optionally something else.
-}
containsElems :: MonadReader StaticImage m => [AbstractPicture] -> m Bool
containsElems ps = asks $ specElems (\t -> all (\p -> t `contains` p) ps) . snd


{- |
True if image contains this subpicture exactly this many times.
-}
thisOften :: MonadReader StaticImage m => AbstractPicture -> Int -> m Bool
thisOften p amount = asks $ specElems (\ps -> count p ps == amount) . snd


{- |
True if image contains this subpicture at least this many times.
-}
atLeast :: MonadReader StaticImage m => AbstractPicture -> Int -> m Bool
atLeast p amount = asks $ specElems (\ps -> count p ps >= amount) . snd


{- |
True if image contains this subpicture at most many times.
-}
atMost :: MonadReader StaticImage m => AbstractPicture -> Int -> m Bool
atMost p amount = asks $ specElems (\ps -> count p ps <= amount) . snd


{- |
True if amount of times this subpicture is contained in the image lies in the specified range.
-}
inRangeOf
  :: MonadReader StaticImage m
  => AbstractPicture
  -> (Int,Int)
  -> m Bool
inRangeOf p (lower,upper) = asks $ specElems (
  \ps -> let occurs = count p ps in occurs >= lower && occurs <= upper
  ) . snd


{- |
Runs the first predicate p, then the second q if p evaluated to `True`.
Does not run q if p evaluates to `False`.
-}
ifThen :: MonadReader env m => m Bool -> m Bool -> m Bool
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
hasRelation :: MonadReader StaticImage m => SpatialQuery -> m Bool
hasRelation q = asks $ specPosition q . snd


{- |
Returns the animation environment with its output mapped over by the argument.
-}
mapAnimation
  :: MonadReader Animation m
  => (StaticImage -> a)
  -> m (Double -> a)
mapAnimation = asks . fmap


{- |
Samples the animation at the given time point and applies a predicate or query to it.
-}
atTime :: Double -> ReaderT StaticImage m a -> ReaderT Animation m a
atTime time = withReaderT ($ time)


{- |
Samples the animation at multiple time points and applies a predicate or query to each image,
then returns a list of results.
-}
queryAt
  :: Applicative m
  => [Double]
  -> ReaderT StaticImage m a
  -> ReaderT Animation m [a]
queryAt frames = for frames . flip atTime


{- |
Samples the animation at multiple time points
and applies a predicate dependent on the current time point to each image.
Returns 'True' if all samples satisfied the predicate.
-}
queryAtWithTime
  :: Applicative m
  => [Double]
  -> (Double -> ReaderT StaticImage m a)
  -> ReaderT Animation m [a]
queryAtWithTime frames action = for frames $ \t -> atTime t (action t)


{- |
Samples the animation at multiple time points and applies a predicate to each image.
Returns 'True' if all samples satisfied the predicate.
-}
allAt :: [Double] -> Reader StaticImage Bool -> Reader Animation Bool
allAt frames = fmap and . queryAt frames


{- |
Samples the animation at multiple time points
and applies a predicate dependent on the current time point to each image.
Returns 'True' if all samples satisfied the predicate.
-}
allAtWithTime
  :: [Double]
  -> (Double -> Reader StaticImage Bool)
  -> Reader Animation Bool
allAtWithTime frames = fmap and . queryAtWithTime frames


{- |
Samples the animation at multiple time points and applies a predicate to each image.
Returns 'True' if any sample satisfied the predicate.
-}
anyAt :: [Double] -> Reader StaticImage Bool -> Reader Animation Bool
anyAt frames = fmap or . queryAt frames


{- |
Samples the animation at multiple time points and applies a predicate to each image.
Returns 'True' if none of the samples satisfied the predicate.
-}
noneAt :: [Double] -> Reader StaticImage Bool -> Reader Animation Bool
noneAt frames = fmap not . anyAt frames



{- |
Samples the animation at multiple time points and returns a list of unmodified results.
-}
rawImagesAt :: Monad m => [Double] -> ReaderT Animation m [Picture]
rawImagesAt frames = queryAt frames rawImage


{- |
Samples the animation at multiple time points and returns a list of normalized results.
-}
normalizedImagesAt :: Monad m => [Double] -> ReaderT Animation m [Picture]
normalizedImagesAt frames = queryAt frames normalizedImage


{- |
Builds a fallible test given an error message and a predicate.
-}
complain :: String -> Reader env Bool -> ReaderT env (Except String) ()
complain label action = do
  environment <- ask
  unless (runReader action environment) $ throwError label


{- |
Executes the given test suite on a static image and returns the error message of the first failed test
or the empty String if all tests passed.
-}
testPicture :: Picture -> ReaderT StaticImage (Except String) () -> String
testPicture p = fromLeft "" . runExcept . flip runReaderT (p, getComponents p)


{- |
Executes the given test suite on an animation and returns the error message of the first failed test
or the empty String if all tests passed.
-}
testAnimation :: (Double -> Picture) -> ReaderT Animation (Except String) () -> String
testAnimation p = fromLeft "" . runExcept . flip runReaderT (\t -> (p t, getComponents $ p t))
