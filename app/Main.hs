module Main (main) where

import Data.Ord (comparing)
import Data.Foldable (foldl', maximumBy)
import LongestAdjacentWord.AreAdjacent (isWordAllAdjacent)

main :: IO ()
main = do
    dictionary <- readFile "/home/caleb/longest-adjacent-word/app/dictionary.txt"
    putStrLn $ foldl' longerAdjacent "" $ lines dictionary

longer :: [String] -> String
longer = maximumBy $ comparing length

longerAdjacent :: String -> String -> String
longerAdjacent longest current = if isWordAllAdjacent current
    then longer [longest, current]
    else longest
