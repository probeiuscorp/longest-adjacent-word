module Main (main) where

import Test.Hspec
import LongestAdjacentWord.AreAdjacent (isWordAllAdjacent)

main :: IO ()
main = hspec $ do
    describe "isWordAllAdjacent" $ do
        it "qwertyuiop" $ do
            isWordAllAdjacent "qwertyuiop" `shouldBe` True
        it "asdfghjkl" $ do
            isWordAllAdjacent "asdfghjkl" `shouldBe` True
        it "zxcvbnm" $ do
            isWordAllAdjacent "zxcvbnm" `shouldBe` True
        it "qazxswedcfrtgbnhyujmkiolp" $ do
            isWordAllAdjacent "qazxswedcfrtgbnhyujmkiolp" `shouldBe` True
        it "qqaazzxxsswweeddccffrrttggbbnnhhyyuujjmmkkiioollpp" $ do
            isWordAllAdjacent "qqaazzxxsswweeddccffrrttggbbnnhhyyuujjmmkkiioollpp" `shouldBe` True
        it "qe" $ do
            isWordAllAdjacent "qe" `shouldBe` False
        it "somebody" $ do
            isWordAllAdjacent "somebody" `shouldBe` False
        it "qs" $ do
            isWordAllAdjacent "qs" `shouldBe` False
        it "sc" $ do
            isWordAllAdjacent "sc" `shouldBe` False
