
{-# language TypeApplications #-}

module CodeWorld.Tasks.Compare (
  testCSE
) where


import Data.Char                        (isNumber, toLower, toUpper)
import Data.List.Extra                  (intercalate, maximumOn, minimumOn, replace)
import Data.Maybe                       (fromJust)
import Data.Tuple.Extra                 (second, both)
import qualified Data.IntMap            as IM

import CodeWorld                        (Picture)
import CodeWorld.Tasks.HashCons         (BiMap, Node(..), hashconsShare)
import CodeWorld.Tasks.Reify            (ReifyPicture(..), share, toInterface)



testCSE :: Picture -> IO (Maybe String)
testCSE p = do
  reifyResult <- share p
  let
    (explicitShares,termIndex) = both IM.toList reifyResult
    (allShares,consTerms) = both toReify $ hashconsShare $ toInterface p
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
        , "Consider this expression resembling your submission, possibly differing in the following ways:"
        , "  - Subexpressions distributed over multiple definitions have been combined into a single expression"
        , "  - Mathematical subexpressions have been fully evaluated"
        , "  - Some picture related subexpressions might also be fully or partially evaluated."
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
        , "The highlighted terms can be defined globally or locally at their use-site."
        , "For a local definition, you can use either a 'let' or 'where' binding."
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
        , addIndentLevel term
        ]


bindMapping :: [(Int,ReifyPicture Int)] -> [(Int,ReifyPicture Int)] -> [(Int,String)]
bindMapping sharedTerms allTerms = map toName (filter (`elem` sharedTerms) allTerms)
  where
    toName = second (formatBinding . ('a':) . capitalFirst . printOriginal [] allTerms)

    formatBinding = camelCase . filter keep . replace "&" "And"

    keep c = c `notElem` ['(',')',',','[',']','-','.','\n'] && not (isNumber c)

    capitalFirst [] = []
    capitalFirst (x:xs) = toUpper x : xs

    camelCase "" = ""
    camelCase " " = ""
    camelCase (' ':' ':s) = camelCase $ ' ' : s
    camelCase (' ':c:s) = toUpper c : camelCase s
    camelCase (c:s) = c : camelCase s

printOriginal :: [(Int,String)] -> [(Int, ReifyPicture Int)] -> ReifyPicture Int -> String
printOriginal bindings termLookup term = case term of
  Color c i             -> unwords
                             [ "colored"
                             , map toLower (parensShow c)
                             , printNext i
                             ]
  Translate x y i       -> unwords
                             [ "translated"
                             , truncatedShow x
                             , truncatedShow y
                             , printNext i
                             ]
  Scale x y i           -> unwords
                             [ "scaled"
                             , truncatedShow x
                             , truncatedShow y
                             , printNext i
                             ]
  Dilate fac i          -> unwords ["dilated", truncatedShow fac, printNext i]
  Rotate a i            -> unwords ["rotated", truncatedShow a, printNext i]
  Reflect a i           -> unwords ["reflected", truncatedShow a, printNext i]
  Clip x y i            -> unwords
                             [ "clipped"
                             , truncatedShow x
                             , truncatedShow y
                             , printNext i
                             ]
  Pictures is           -> unwords
                             [ "pictures ["
                             , intercalate ", " (map printNextAnd is)
                             , "]"
                             ]
  And i1 i2             -> printNextAnd i1 ++ " &\n" ++ printNextAnd i2
  Rectangle x y         -> unwords ["rectangle", truncatedShow x, truncatedShow y]
  ThickRectangle t x y  -> unwords
                             [ "thickRectangle"
                             , truncatedShow t
                             , truncatedShow x
                             , truncatedShow y
                             ]
  SolidRectangle x y    -> unwords
                             [ "solidRectangle"
                             , truncatedShow x
                             , truncatedShow y
                             ]
  Circle r              -> "circle " ++ truncatedShow r
  ThickCircle t r       -> unwords
                             [ "thickCircle"
                             , truncatedShow t
                             , truncatedShow r
                             ]
  SolidCircle r         -> "solidCircle " ++ truncatedShow r
  Polygon ps            -> "polygon " ++ show ps
  ThickPolygon t ps     -> unwords ["thickPolygon", truncatedShow t, show ps]
  SolidPolygon ps       -> "solidPolygon " ++ show ps
  Polyline ps           -> "polyline " ++ show ps
  ThickPolyline t ps    -> unwords ["thickPolyline", truncatedShow t, show ps]
  Sector a1 a2 r        -> unwords
                             [ "sector"
                             , truncatedShow a1
                             , truncatedShow a2
                             , truncatedShow r
                             ]
  Arc a1 a2 r           -> unwords
                             [ "arc"
                             , truncatedShow a1
                             , truncatedShow a2
                             , truncatedShow r
                             ]
  ThickArc t a1 a2 r    -> unwords
                             [ "thickArc"
                             , truncatedShow t
                             , truncatedShow a1
                             , truncatedShow a2
                             , truncatedShow r
                             ]
  Curve ps              -> "curve " ++ show ps
  ThickCurve t ps       -> unwords ["curve", truncatedShow t, show ps]
  ClosedCurve ps        -> "closedCurve " ++ show ps
  ThickClosedCurve t ps -> unwords ["thickClosedCurve", truncatedShow t, show ps]
  SolidClosedCurve ps   -> "solidClosedCurve " ++ show ps
  Lettering t           -> "lettering " ++ show t
  StyledLettering s f t -> unwords ["styledLettering", show s, show f, show t]
  CoordinatePlane       -> "coordinatePlane"
  Logo                  -> "codeWorldLogo"
  Blank                 -> "blank"

  where
    printNext :: Int -> String
    printNext i = case lookup i bindings of
      Nothing
          | hasArguments reifyPic -> result
          | otherwise             -> originalTerm
        where reifyPic = fromJust $ lookup i termLookup
              originalTerm = printOriginal bindings termLookup reifyPic
              result = if '&' `elem` originalTerm || length originalTerm > maxLineWidth
                then "(\n" ++ addIndentLevel originalTerm ++ ")"
                else "(" ++ originalTerm ++ ")"
      Just name -> name

    printNextAnd :: Int -> String
    printNextAnd i = case lookup i bindings of
      Nothing -> printOriginal bindings termLookup $ fromJust $ lookup i termLookup
      Just name -> name

    roundTo :: Integer -> Double -> Double
    roundTo places d = (fromIntegral @Int . round $ d * fac) / fac
      where fac = 10^places

    parensShow :: Show a => a -> String
    parensShow = optParens . show

    truncatedShow = optParens . reSubPi . show . roundTo 3

    optParens s
      | length (words s) > 1  || any (`elem` s) "-/*" = '(': s ++ ")"
      | otherwise = s

    reSubPi "3.141" = "pi"
    reSubPi "1.570" = "pi/2"
    reSubPi "0.785" = "pi/4"
    reSubPi "4.712" = "3*pi/2"
    reSubPi "2.356" = "3*pi/4"
    reSubPi ('-':s) = '-': reSubPi s
    reSubPi s       = s


addIndentLevel :: String -> String
addIndentLevel = unlines . map ("  " ++) . lines


maxLineWidth :: Int
maxLineWidth = 80


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
