module Main where

import System.Environment
import Data.List.Split
import Data.List
import Data.List.Utils (replace)
import Data.Char (isSpace)
import Control.Monad
import System.Exit
import System.Directory

strip :: String -> String
strip = f . f where f = reverse . dropWhile isSpace

parse :: String -> [(String, [[String]])]
parse content = do
  let notNull = filter (not . null)
  let nonLinesCommentOfFile = filter (\x -> head x /= '#') $ notNull $ lines content
  let nonCommentFile = concat $ intersperse "\n" nonLinesCommentOfFile
  let nonExtendedFile = replace (concat ["\\", "\n"]) "" nonCommentFile
  let linesOfFile = notNull $ splitOn "[" nonExtendedFile
  let lineWHeaders = [notNull $ splitOn "]" x | x <- linesOfFile]
  let fmnt a = notNull [[strip y | y <- notNull $ splitOn "=" x] | x <- lines a]
  let cnv = map (\[name, amount] -> (name, fmnt amount)) . chunksOf 2
  let filteredLines = [head $ cnv x | x <- lineWHeaders]
  filteredLines

export :: [(String, [[String]])] -> String
export structure = do
  let createSection name values = concat [concat ["[", name, "]\n"], concat [concat [a !! 0, " = ", a !! 1, "\n"] | a <- values]]
  let totalFile = concat $ [createSection (fst a) (snd a) | a <- structure] ++ ["\n"]
  totalFile

main :: IO()
main = do
  args <- getArgs
  --print args
  when (length args == 0) $ exitSuccess
  --when (getEnv "HOME" ==  null) $ exitFailure
  home <- getEnv "HOME"
  let filename = concat [home, "/.config/wayfire.ini"]
  let filename_bak = concat [filename, ".bak"]
  when (("-p" == args !! 0) && (length args == 1)) $ do
    contents <- readFile filename
    let parsed = parse contents
    putStr $ export parsed
    exitSuccess
  when (("-o" == args !! 0) && (length args == 2)) $ do
    contents <- readFile filename
    let parsed = parse contents
    let oFilename = args !! 1
    let reConstructed = export parsed
    writeFile oFilename reConstructed
    exitSuccess
  when (("-g" == args !! 0) && (length args == 3)) $ do
    contents <- readFile filename
    let parsed = parse contents
    let [sec, name] = [args !! 1, args !! 2]
    let correctSec = [snd x | x <- parsed, fst x == sec] !! 0
    let correctName = [x !! 1 | x <- correctSec, x !! 0 == name]
    putStrLn $ concat correctName
    exitSuccess
  let alterParse sec name newVal parsed = [(fst x, [if (y !! 0 == name) && (fst x == sec) then [y !! 0, newVal] else y | y <- snd x]) | x <- parsed] --took a while to find
  when (("-c" == args !! 0) && (length args == 4)) $ do
    contents <- readFile filename
    let parsed = parse contents
    let [sec, name, newVal] = [args !! 1, args !! 2, args !! 3]
    let alteredParse = alterParse sec name newVal parsed
    let reConstructed = export alteredParse
    putStr reConstructed
    exitSuccess
  when (("-c" == args !! 0) && (length args == 5)) $ do
    contents <- readFile filename
    let parsed = parse contents
    let [sec, name, newVal, oFilename] = [args !! 1, args !! 2, args !! 3, args !! 4]
    let alteredParse = alterParse sec name newVal parsed
    let reConstructed = export alteredParse
    writeFile oFilename reConstructed
    exitSuccess
  backup_exists <- doesFileExist filename_bak
  when (("-cx" == args !! 0) && (length args == 4) && (not backup_exists)) $ do
    copyFile filename filename_bak
    contents <- readFile filename_bak
    let parsed = parse contents
    let [sec, name, newVal] = [args !! 1, args !! 2, args !! 3]
    let alteredParse = alterParse sec name newVal parsed
    let reConstructed = export alteredParse
    writeFile filename reConstructed
    exitSuccess
  when (("-b" == args !! 0) && (length args == 1) && backup_exists) $ do
    copyFile filename_bak filename
    removeFile filename_bak
    exitSuccess
  exitFailure
