module LongestAdjacentWord.AdjacentPairs (adjacentPairs) where

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