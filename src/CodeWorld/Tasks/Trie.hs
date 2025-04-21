{-# language OverloadedStrings #-}
{-# options_ghc -Wno-orphans #-}

module CodeWorld.Tasks.Trie (
  shareGraph,
  ) where


import Control.Monad.Identity           (Identity)
import Control.Monad.State              (State, get, put, runState)
import Data.ByteString                  (ByteString)
import Data.List                        (intersperse)
import Data.Text.Encoding               (encodeUtf8)
import Data.Trie                        (Trie)
import qualified Data.ByteString.Char8  as BS
import qualified Data.Trie              as T

import CodeWorld.Tasks.API              (Drawable(..))
import CodeWorld.Tasks.HashCons         (Node(..), NodeId, getNodes)



data DAG = DAG {
  unTrie :: Trie (NodeId,Node),
  maximumId :: NodeId
} deriving Show


data Graph = Graph {
  unGraph :: State DAG NodeId,
  unStringAST :: ByteString
  }


instance MonadFail Identity where
  fail = error "Computation Didn't return expected amount of arguments."


instance Semigroup Graph where
  (<>) = (&)


instance Monoid Graph where
  mempty  = blank
  mconcat = pictures


instance Drawable Graph where

  blank                = processSimple BlankNode
  coordinatePlane      = processSimple CoordinatePlaneNode
  codeWorldLogo        = processSimple LogoNode
  rectangle x          = processSimple . RectangleNode x
  solidRectangle x     = processSimple . SolidRectangleNode x
  thickRectangle t x   = processSimple . ThickRectangleNode t x
  circle               = processSimple . CircleNode
  solidCircle          = processSimple. SolidCircleNode
  thickCircle t        = processSimple. ThickCircleNode t
  arc a1 a2            = processSimple. ArcNode a1 a2
  sector a1 a2         = processSimple. SectorNode a1 a2
  thickArc t a1 a2     = processSimple. ThickArcNode t a1 a2
  curve                = processSimple. CurveNode
  thickCurve t         = processSimple. ThickCurveNode t
  closedCurve          = processSimple. ClosedCurveNode
  thickClosedCurve t   = processSimple. ThickClosedCurveNode t
  solidClosedCurve     = processSimple. SolidClosedCurveNode
  lettering            = processSimple . LetteringNode
  styledLettering fs t = processSimple . StyledLetteringNode fs t
  polyline             = processSimple . PolylineNode
  thickPolyline t      = processSimple . ThickPolylineNode t
  polygon              = processSimple . PolygonNode
  solidPolygon         = processSimple . SolidPolygonNode
  thickPolygon t       = processSimple . ThickPolygonNode t

  translated x y = processOneGraph $ TranslateNode x y
  colored c      = processOneGraph $ ColorNode c
  dilated d      = processOneGraph $ DilateNode d
  scaled x y     = processOneGraph $ ScaleNode x y
  rotated a      = processOneGraph $ RotateNode a
  reflected a    = processOneGraph $ ReflectNode a
  clipped x y    = processOneGraph $ ClipNode x y

  pictures ps = Graph sT sAST
    where
      sAST = buildStringAST (PicturesNode undefined) $ map unStringAST ps
      sT = do
        n <- seqArgs ps
        triecons sAST $ PicturesNode n

  p & q = Graph sT sAST
    where
      sAST = buildStringAST (AndNode undefined undefined) $ map unStringAST [p,q]
      sT = do
        [e1,e2] <- seqArgs [p,q]
        triecons sAST $ AndNode e1 e2


processSimple :: Node -> Graph
processSimple node = Graph (triecons sAST node) sAST
  where
    sAST = buildStringAST node []


processOneGraph :: (NodeId -> Node) -> Graph -> Graph
processOneGraph partialNode graph = Graph sT sAST
    where
      sAST = buildStringAST (partialNode undefined) [unStringAST graph]
      sT = do
        [e] <- seqArgs [graph]
        triecons sAST $ partialNode e


triecons :: ByteString -> Node -> State DAG NodeId
triecons sAST node = do
  DAG trie maxId <- get
  case T.lookup sAST trie of
    Nothing -> do
      let
        maxId' = maxId+1
        trie' = T.insert sAST (maxId',node) trie
      put $ DAG trie' maxId'
      pure maxId'
    Just (nodeId,_) -> pure nodeId


seqArgs :: [Graph] -> State DAG [NodeId]
seqArgs = mapM seqArg
  where
    seqArg (Graph sT sAST) = do
      DAG trie _ <- get
      case T.lookup sAST trie of
        Nothing -> sT
        Just (nodeID,_) -> pure nodeID


buildStringAST :: Node -> [ByteString] -> ByteString
buildStringAST node args = opString <> argsString
  where
    toBS :: Show a => a -> ByteString
    toBS = BS.pack . show

    opString = case node of
      RectangleNode x y -> "rectangle " <> toBS x <> " " <> toBS y
      ThickRectangleNode t x y -> "thickRectangle " <> toBS t <> " " <> toBS x <> " " <> toBS y
      SolidRectangleNode x y -> "solidRectangle " <> toBS x <> " " <> toBS y
      CircleNode r -> "circle " <> toBS r
      ThickCircleNode t r -> "thickCircle " <> toBS t <> " " <> toBS r
      SolidCircleNode r -> "solidCircle " <> toBS r
      CurveNode ps -> "curve " <> toBS ps
      ThickCurveNode t ps -> "thickCurve " <> toBS t <> " " <> toBS ps
      ClosedCurveNode ps -> "closedCurve " <> toBS ps
      ThickClosedCurveNode t ps -> "thickClosedCurve " <> toBS t <> " " <> toBS ps
      SolidClosedCurveNode ps -> "solidClosedCurve " <> toBS ps
      ArcNode a1 a2 r -> "arc " <> toBS a1 <> " " <> toBS a2 <> " " <> toBS r
      ThickArcNode t a1 a2 r -> "thickArc " <> toBS t <> " " <> toBS a1 <> " " <> toBS a2 <> " " <> toBS r
      SectorNode a1 a2 r -> "sector " <> toBS a1 <> " " <> toBS a2 <> " " <> toBS r
      PolylineNode ps -> "polyline " <> toBS ps
      ThickPolylineNode t ps -> "thickPolyline " <> toBS t <> " " <> toBS ps
      PolygonNode ps -> "polygon " <> toBS ps
      ThickPolygonNode t ps -> "polygon " <> toBS t <> " " <> toBS ps
      SolidPolygonNode ps -> "solidPolygon " <> toBS ps
      LetteringNode t -> "lettering " <> encodeUtf8 t
      StyledLetteringNode ts f t -> "styledLettering " <> toBS ts <> " " <> toBS f <> " " <> encodeUtf8 t
      ColorNode c _ -> "color " <> toBS c
      TranslateNode x y _ -> "translated " <> toBS x <> " " <> toBS y
      ScaleNode x y _ -> "scaled " <> toBS x <> " " <> toBS y
      DilateNode d _ -> "dilated " <> toBS d
      RotateNode a _ -> "rotated " <> toBS a
      ReflectNode a _ -> "reflected " <> toBS a
      ClipNode x y _ -> "clipped " <> toBS x <> " " <> toBS y
      PicturesNode _ -> "pictures"
      AndNode _ _ -> "(&)"
      CoordinatePlaneNode -> "coordinatePlane"
      LogoNode -> "codeWorldLogo"
      BlankNode -> "blank"
    argsString = case args of
      [] -> ""
      _  -> " (" <> BS.concat (intersperse ") (" args) <> ")"


buildDAG :: Graph -> (NodeId, DAG)
buildDAG g = runState (unGraph g) (DAG T.empty 0)


shareGraph :: Graph -> ([Node], [(NodeId, Node)])
shareGraph g = (map shared multi, nodeIdPairs)
  where
    DAG trie mId = snd $ buildDAG g
    nodeIdPairs = T.elems trie
    nodeIds = concatMap (getNodes . snd) nodeIdPairs
    count x = length . filter (==x)
    multi = filter ((> 1) . flip count nodeIds) [1..mId]
    shared x = case lookup x nodeIdPairs of
      Just r -> r
      _      -> error "impossible"
