module Main (main, isWordAllAdjacent) where

import qualified Data.Set as Set
import Data.Ord (comparing)
import Data.Foldable (foldl', maximumBy)

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

isWordAllAdjacent :: String -> Bool
isWordAllAdjacent (a:(b:xs)) = areAdjacent (Set.fromList [a,b]) && isWordAllAdjacent (b:xs)
isWordAllAdjacent _ = True

adjacent :: [Set.Set Char]
adjacent = map (\(a, b) -> Set.fromList [a,b]) adjacentPairs

areAdjacent :: Set.Set Char -> Bool
areAdjacent chars = any (== chars) adjacent

adjacentPairs :: [(Char, Char)]
adjacentPairs =
    -- First row
    [ ('q', 'w')
    , ('e', 'w')
    , ('e', 'r')
    , ('t', 'r')
    , ('t', 'y')
    , ('u', 'y')
    , ('u', 'i')
    , ('o', 'i')
    , ('o', 'p')
    -- Second row
    , ('a', 's')
    , ('d', 's')
    , ('d', 'f')
    , ('g', 'f')
    , ('g', 'h')
    , ('j', 'h')
    , ('j', 'k')
    , ('l', 'k')
    -- Third row
    , ('z', 'x')
    , ('c', 'x')
    , ('c', 'v')
    , ('b', 'v')
    , ('b', 'b')
    , ('m', 'b')
    -- First-to-second
    , ('q', 'a')
    , ('q', 's')
    , ('w', 'a')
    , ('w', 's')
    , ('w', 'd')
    , ('e', 's')
    , ('e', 'd')
    , ('e', 'f')
    , ('r', 'd')
    , ('r', 'f')
    , ('r', 'g')
    , ('t', 'f')
    , ('t', 'g')
    , ('t', 'h')
    , ('y', 'g')
    , ('y', 'h')
    , ('y', 'j')
    , ('u', 'h')
    , ('u', 'j')
    , ('u', 'k')
    , ('i', 'j')
    , ('i', 'k')
    , ('i', 'l')
    , ('o', 'k')
    , ('o', 'l')
    , ('o', 'p')
    , ('l', 'p')
    -- Second-to-third
    , ('a', 'z')
    , ('s', 'z')
    , ('s', 'x')
    , ('d', 'x')
    , ('d', 'c')
    , ('f', 'c')
    , ('f', 'v')
    , ('g', 'v')
    , ('g', 'b')
    , ('h', 'b')
    , ('h', 'n')
    , ('j', 'n')
    , ('j', 'm')
    , ('k', 'm') ]