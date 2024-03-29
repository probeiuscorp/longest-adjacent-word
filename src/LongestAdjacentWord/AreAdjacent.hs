module LongestAdjacentWord.AreAdjacent (isWordAllAdjacent) where

import qualified Data.Set as Set
import LongestAdjacentWord.AdjacentPairs (adjacentPairs)

isWordAllAdjacent :: String -> Bool
isWordAllAdjacent (a:(b:xs)) = areAdjacent (Set.fromList [a,b]) && isWordAllAdjacent (b:xs)
isWordAllAdjacent _ = True

adjacent :: [Set.Set Char]
adjacent = map (\(a, b) -> Set.fromList [a,b]) adjacentPairs

areAdjacent :: Set.Set Char -> Bool
areAdjacent chars = hasOneElement chars || any (== chars) adjacent

hasOneElement :: Set.Set a -> Bool
hasOneElement set = Set.size set == 1
