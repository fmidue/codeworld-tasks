 {-# LANGUAGE OverloadedStrings #-}

module Main where


import qualified Data.Text              as T
import qualified Data.Text.Lazy         as LT

import Data.Int                         (Int64)
import Data.Text                        (Text)
import Data.Text.Lazy.Builder           (toLazyText)
import Haskell.Template.Task            (grade)
import Rainbow (
  Chunk,
  back,
  fore,
  bold,
  chunk,
  brightBlue,
  brightRed,
  green,
  red,
  yellow,
  putChunkLn,
  )
import System.Directory                 (getTemporaryDirectory)
import System.Exit                      (die, exitFailure, exitSuccess)
import System.Environment               (getArgs)
import System.FilePath                  ((-<.>))
import System.IO                        (readFile')
import Text.PrettyPrint.Leijen.Text     (Doc, SimpleDoc(..), renderPretty)



main :: IO ()
main = do
  args <- getArgs
  case args of
    [task,submission] -> do
        taskContents <- openDotHs task
        submissionContents <- openDotHs submission
        runTemplateTask taskContents submissionContents
        exitSuccess
    _                 -> die usage


runTemplateTask :: String -> String -> IO ()
runTemplateTask task submission = do
  tmp <- getTemporaryDirectory
  grade
    id
    rejection
    suggestion
    tmp
    task
    submission
  putChunkLn $ bold $ back green "SUCCESS"


openDotHs :: FilePath -> IO String
openDotHs path = readFile' $ path -<.> "hs"


usage :: String
usage = "usage: test-task <config path> <solution path>"


display :: SimpleDoc -> [Text]
display SEmpty        = [""]
display (SChar _ _)   = []
display (SText _ s x) = LT.toStrict (toLazyText s) : display x
display (SLine i x)   = newLine : display x
  where
    newLine = "\n" <> spaces i


spaces :: Int64 -> Text
spaces n
  | n <= 0 = ""
  | otherwise = LT.toStrict $ LT.replicate n (LT.singleton ' ')


formatRejections :: Text -> Chunk
formatRejections t
  | "Unless you fix the above" `T.isPrefixOf` t = bold $ fore red $ chunk t
  | otherwise = chunk t


formatSuggestions :: Text -> Chunk
formatSuggestions t
  | "Solution" `T.isPrefixOf` t = bold $ fore brightRed $ chunk t
  | t `elem` ["Found:","Perhaps:"] = bold $ fore brightBlue $ chunk t
  | otherwise = bold $ chunk t


suggestion :: Doc -> IO ()
suggestion doc = do
  putChunkLn $ bold $ back yellow "SUGGESTION:"
  emptyLine
  unlinesChunks $ map formatSuggestions $ toLines doc
  emptyLine


rejection :: Doc -> IO a
rejection doc = do
  putChunkLn $ bold $ back red "ERROR:"
  emptyLine
  unlinesChunks $ map formatRejections $ toLines doc
  emptyLine
  putChunkLn $ bold $ back red "REJECTED"
  exitFailure


emptyLine :: IO ()
emptyLine = putStrLn ""


toLines :: Doc -> [Text]
toLines = concatMap T.lines . display . renderPretty 0.4 80


unlinesChunks :: [Chunk] -> IO ()
unlinesChunks = mapM_ putChunkLn
