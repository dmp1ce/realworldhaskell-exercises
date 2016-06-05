module DirectionSpec where

import Test.Hspec
import Test.Hspec.QuickCheck

import Direction  (
                    calculateDirection
                  , Point(..)
                  , Direction(..)
                  , successiveTripleDirections
                  , grahamScanAlgo
                  , getLowestPoint
                  , getSecondPoint
                  , buildHull
                  )

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Direction Exercies from Real World Haskell" $ do
    
    -- Define some points for tests
    let a = Point 1 2
    let b = Point 2 2
    let c = Point 2 3
    let d = Point 3 2
    let e = Point 1 1
    let f = Point (-1) (-2)

    -- Graham Scan Algorithm tests
    it "Simple triangle hull" $ do
      grahamScanAlgo [a,b,c] `shouldBe` [a,b,c]
    it "Square hull with point inside" $ do
      grahamScanAlgo [a,b,c,d,e] `shouldBe` [e,d,c,a]
    it "Get lowest point from quad 1" $ do 
      getLowestPoint [a,b,c,d,e] `shouldBe` e
    it "Get lowest point from quad 4" $ do 
      getLowestPoint [a,b,c,d,e,f] `shouldBe` f
    it "Get second point" $ do
      getSecondPoint f [a,b,c,d,e,f] `shouldBe` d
    it "Get second point different Point order" $ do
      getSecondPoint f [a,b,c,f,d,e] `shouldBe` d
    it "Get second point past errors from points" $ do
      getSecondPoint e [a,b,c,d,e] `shouldBe` d
    it "build hull works" $ do
      buildHull [a,b,c,d,e] [e,d] `shouldBe` [e,d,c,a]

    it "calculateDirection Left works" $ do
      calculateDirection a b c `shouldBe` Just MyLeft
    it "calculateDirection Right works" $ do
      calculateDirection a c b `shouldBe` Just MyRight
    it "calculateDirection Straight works" $ do
      calculateDirection a b d `shouldBe` Just MyStraight
    it "calculateDirection Left works 2" $ do
      calculateDirection b c d `shouldBe` Just MyRight
    prop "no direction" $ \a' ->
      calculateDirection a' a' a' `shouldBe` Nothing 
    prop "no direction 2" $ \a' b' ->
      calculateDirection a' a' b' `shouldBe` Nothing 
    prop "no direction 3" $ \a' b' ->
      calculateDirection a' b' a' `shouldBe` Nothing 
    it "successiveTripleDirections works" $ do
      successiveTripleDirections [a,b,c,d] `shouldBe`
        [Just MyLeft, Just MyRight]
