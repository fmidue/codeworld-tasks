{-# language RankNTypes #-}

module CodeWorld.Tasks.Compare (
  testCSE,
) where


import Data.Char                        (toLower, toUpper)
import Data.List.Extra                  (maximumOn, minimumOn, replace)
import Data.Maybe                       (fromJust)
import Data.Tuple.Extra                 (second, both)
import qualified Data.IntMap            as IM

import CodeWorld.Tasks.API              (Picture)
import CodeWorld.Tasks.HashCons         (BiMap, Node(..), hashconsShare)
import CodeWorld.Tasks.Reify            (ReifyPicture(..), share)



testCSE :: Picture -> IO (Maybe String)
testCSE p = do
  reifyResult <- share p
  let
    (explicitShares,termIndex) = both IM.toList reifyResult
    (allShares,consTerms) = both toReify $ hashconsShare p
  if length termIndex == length consTerms
    then
      pure Nothing
    else do
      let
        usedBinds = bindMapping explicitShares termIndex
        possibleBinds = bindMapping allShares consTerms
        completeTerm = restoreTerm usedBinds termIndex $ minimumOn fst termIndex
        explicit = map (restoreTerm usedBinds termIndex) explicitShares
        sharable = map (restoreTerm possibleBinds consTerms) allShares
        completeCons = restoreTerm possibleBinds consTerms $ maximumOn fst consTerms
      pure $ Just $ unlines
        [ "There are opportunities for common subexpression elimination (CSE) in your submission!"
        , "Consider this expression resembling your submission, condensed in the following ways:"
        , "  - Subexpressions distributed over multiple definitions have been combined into a single expression"
        , "  - Mathematical subexpressions have been fully evaluated"
        , "  - Already defined bindings might have been renamed"
        , "  - Used 'where' bindings have been converted to 'let' bindings"
        , "  - Bindings which are not relevant to CSE have been removed"
        , ""
        , printSharedTerm completeTerm $ termsWithNames usedBinds explicitShares explicit
        , ""
        , ""
        , "It could be rewritten like this:"
        , ""
        , printSharedTerm completeCons $ termsWithNames possibleBinds allShares sharable
        , ""
        , ""
        , "If the highlighted terms are already defined globally, then consider locally defining them at their use-site instead."
        , "You can define them with either a 'let' or 'where' binding."
        , "Of course, you can also change the proposed names to your liking, e.g. make them more concise."
        , "Also consider that your actual code is most likely structured slightly differently than this suggested improvement."
        , "As such, the location of the binding as shown here might also have to be adjusted."
        ]
  where
    restoreTerm bindings termLookup = printOriginal bindings termLookup . snd
    termsWithNames bindings shares = zip (map (fromJust . flip lookup bindings . fst) shares)

    printSharedTerm term shared
      | null shared = term
      | otherwise = unlines $
        "let" :
        map (\(name,value) -> "  " ++ name ++ " = " ++ value) shared ++
        [  "in"
        ,  "  " ++ term
        ]


bindMapping :: [(Int,ReifyPicture Int)] -> [(Int,ReifyPicture Int)] -> [(Int,String)]
bindMapping sharedTerms allTerms = map toName (filter (`elem` sharedTerms) allTerms)
  where
    toName = second (formatBinding . printOriginal [] allTerms)

    formatBinding = camelCase . filter (\c -> c `notElem` ['(',')']) . replace "&" "And" . replace "." ""
    camelCase "" = ""
    camelCase (' ':' ':s) = camelCase $ ' ' : s
    camelCase (' ':c:s) = toUpper c : camelCase s
    camelCase (c:s) = c : camelCase s

printOriginal :: [(Int,String)] -> [(Int, ReifyPicture Int)] -> ReifyPicture Int -> String
printOriginal bindings termLookup term = sub
  where
    printNext :: Int -> String
    printNext i = case lookup i bindings of
      Nothing
          | hasArguments reifyPic -> "(" ++ printOriginal bindings termLookup reifyPic ++ ")"
          | otherwise             -> printOriginal bindings termLookup reifyPic
        where reifyPic = fromJust $ lookup i termLookup
      Just name -> name

    printNextAnd :: Int -> String
    printNextAnd i = case lookup i bindings of
      Nothing -> printOriginal bindings termLookup $ fromJust $ lookup i termLookup
      Just name -> name

    sub = unwords $ case term of
      Color c i       -> ["colored", map toLower (show c), printNext i]
      Translate x y i -> ["translated", show x, show y, printNext i]
      Scale x y i     -> ["scaled", show x, show y, printNext i]
      Dilate fac i    -> ["dilated", show fac, printNext i]
      Rotate a i      -> ["rotated", show a, printNext i]
      Reflect a i     -> ["reflected", show a, printNext i]
      Clip x y i      -> ["clipped", show x, show y, printNext i]
      Pictures is     -> ["pictures", concatMap printNext is]
      And i1 i2       -> [printNextAnd i1, "&", printNextAnd i2]
      _               -> case show term of
        (x:xs) -> [toLower x:xs]
        _      -> error "not possible"


hasArguments :: ReifyPicture a -> Bool
hasArguments Blank           = False
hasArguments CoordinatePlane = False
hasArguments Logo            = False
hasArguments _               = True


toReify :: BiMap Node -> [(IM.Key, ReifyPicture Int)]
toReify = map $ second toReifyPic
  where
    toReifyPic n = case n of
      RectangleNode x y -> Rectangle x y
      ThickRectangleNode t x y -> ThickRectangle t x y
      SolidRectangleNode x y -> SolidRectangle x y
      CircleNode r -> Circle r
      ThickCircleNode t r -> ThickCircle t r
      SolidCircleNode r -> SolidCircle r
      PolygonNode ps -> Polygon ps
      SolidPolygonNode ps -> SolidPolygon ps
      ThickPolygonNode t ps -> ThickPolygon t ps
      ClosedCurveNode ps -> ClosedCurve ps
      SolidClosedCurveNode ps -> SolidClosedCurve ps
      ThickClosedCurveNode t ps -> ThickClosedCurve t ps
      PolylineNode ps -> Polyline ps
      ThickPolylineNode t ps -> ThickPolyline t ps
      CurveNode ps -> Curve ps
      ThickCurveNode t ps -> ThickCurve t ps
      SectorNode a1 a2 r -> Sector a1 a2 r
      ArcNode a1 a2 r -> Arc a1 a2 r
      ThickArcNode t a1 a2 r -> ThickArc t a1 a2 r
      LetteringNode t -> Lettering t
      StyledLetteringNode ts f t -> StyledLettering ts f t
      ColorNode c p -> Color c p
      TranslateNode x y p -> Translate x y p
      ScaleNode x y p -> Scale x y p
      DilateNode fac p -> Dilate fac p
      RotateNode a p -> Rotate a p
      ReflectNode a p -> Reflect a p
      ClipNode x y p -> Clip x y p
      PicturesNode ps -> Pictures ps
      AndNode p1 p2 -> And p1 p2
      CoordinatePlaneNode -> CoordinatePlane
      LogoNode -> Logo
      BlankNode -> Blank
