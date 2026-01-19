
module CodeWorld.Test.Solution (
  PicPredicate,
  containsElem,
  containsElems,
  containsExactElems,
  testLabel,
  runTests,
  hasRelation,
  (<||>),
  (<&&>),
  option,
  options,
  ifThen,
  thisOften,
  atLeast,
  atMost,
  inRangeOf,
  findMaybe,
  findAll,
  findAllAnd,
  findMaybeAnd,
  findAllActual,
  findMaybeActual,
  findAllActualAnd,
  findMaybeActualAnd,
  oneOf,
  getComponents,
  ) where

import Control.Monad (unless)
import Control.Monad.Except
import Control.Monad.Reader
import Data.Either (fromLeft)
import Data.Maybe (listToMaybe)

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
import CodeWorld.Test.Rewrite (normalizeAndAbstract)


{- |
Alias for queries on `Components`.
-}
type PicQuery a = Reader Components a

{- |
Alias for predicates on `Components`.
-}
type PicPredicate = PicQuery Bool


{- |
At least one of many predicates evaluates to True.
-}
options :: [PicPredicate] -> PicPredicate
options = foldr (<||>) (pure False)


{- |
At least one of two predicates evaluates to True.
-}
(<||>) :: PicPredicate -> PicPredicate -> PicPredicate
(<||>) p q = (||) <$> p <*> q


{- |
Both predicates evaluate to True.
-}
(<&&>) :: PicPredicate -> PicPredicate -> PicPredicate
(<&&>) p q = (&&) <$> p <*> q


{- |
Alias for (`<||>`)
-}
option :: PicPredicate -> PicPredicate -> PicPredicate
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
findMaybe :: (AbstractPicture -> Bool) -> PicQuery (Maybe AbstractPicture)
findMaybe = fmap listToMaybe . findAll


{- |
Returns all picture elements satisfying the predicate. (translation is removed)
-}
findAll :: (AbstractPicture -> Bool) -> PicQuery [AbstractPicture]
findAll f = asks $ filter f . specElems (map stripTranslation . getSubPictures)


{- |
Returns all subpictures satisfying the predicate. (includes translation)
-}
findAllActual :: (AbstractPicture -> Bool) -> PicQuery [AbstractPicture]
findAllActual f = asks $ filter f . specElems getSubPictures


{- |
Returns the first subpicture satisfying the predicate if it exists. (includes translation)
-}
findMaybeActual :: (AbstractPicture -> Bool) -> PicQuery (Maybe AbstractPicture)
findMaybeActual = fmap listToMaybe . findAllActual


{- |
Finds all subpictures satisfying a predicate, then applies a function. (includes translation)
-}
findAllActualAnd :: (AbstractPicture -> Bool) -> (AbstractPicture -> a) -> PicQuery [a]
findAllActualAnd p f = map f <$> findAllActual p


{- |
Finds the first subpicture satisfying a predicate, then applies a function if it exists. (includes translation)
-}
findMaybeActualAnd :: (AbstractPicture -> Bool) -> (AbstractPicture -> a) -> PicQuery (Maybe a)
findMaybeActualAnd p = fmap listToMaybe . findAllActualAnd p


{- |
Finds all picture elements satisfying a predicate, then applies a function. (translation is removed)
-}
findAllAnd :: (AbstractPicture -> Bool) -> (AbstractPicture -> a) -> PicQuery [a]
findAllAnd f g = map g <$> findAll f


{- |
Finds the first element satisfying a predicate, then applies a function if it exists. (translation is removed)
-}
findMaybeAnd :: (AbstractPicture -> Bool) -> (AbstractPicture -> a) -> PicQuery (Maybe a)
findMaybeAnd f = fmap listToMaybe . findAllAnd f


{- |
True if image contains exactly these subpictures and nothing else.
-}
containsExactElems :: [AbstractPicture] -> PicPredicate
containsExactElems ps = asks $ specElems (all (\tp -> Pictures ps `contains` tp) . getSubPictures)

{- |
True if image contains at least this subpicture and optionally something else.
-}
containsElem :: AbstractPicture -> PicPredicate
containsElem p = asks $ specElems (`contains` p)

{- |
True if image contains at least these subpictures and optionally something else.
-}
containsElems :: [AbstractPicture] -> PicPredicate
containsElems ps = asks $ specElems (\t -> all (\p -> t `contains` p) ps)


{- |
True if image contains this subpicture exactly this many times.
-}
thisOften :: AbstractPicture -> Int -> PicPredicate
thisOften p amount = asks $ specElems (\ps -> count p ps == amount)


{- |
True if image contains this subpicture at least this many times.
-}
atLeast :: AbstractPicture -> Int -> PicPredicate
atLeast p amount = asks $ specElems (\ps -> count p ps >= amount)


{- |
True if image contains this subpicture at most many times.
-}
atMost :: AbstractPicture -> Int -> PicPredicate
atMost p amount = asks $ specElems (\ps -> count p ps <= amount)


{- |
True if amount of times this subpicture is contained in the image lies in the specified range.
-}
inRangeOf :: AbstractPicture -> (Int,Int) -> PicPredicate
inRangeOf p (lower,upper) = asks $ specElems (\ps -> let occurs = count p ps in occurs >= lower && occurs <= upper)


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
hasRelation = asks . specPosition


{- |
Builds a fallible test given an error message and a predicate.
-}
testLabel :: String -> PicPredicate -> ReaderT Components (Except String) ()
testLabel label r = do
  c <- ask
  unless (runReader r c) $ throwError label


{- |
Executes the given test suite and returns the error message of the first failed test
or the empty String if all tests passed.
-}
runTests :: Picture -> ReaderT Components (Except String) () -> String
runTests p = fromLeft "" . runExcept . flip runReaderT (getComponents p)
