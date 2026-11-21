{-# LANGUAGE OverloadedStrings #-}

module StearnsWharf.NodeSpec
  ( spec,
  )
where

import StearnsWharf.Common
import StearnsWharf.Node
import Test.Hspec

n1 :: Node
n1 =
  Node
    { nodeId = 1,
      nx = 0.0,
      ny = 0.0,
      dof = Dof 0 0 1,
      globNdx = 0
    }

n2 :: Node
n2 =
  Node
    { nodeId = 2,
      nx = 5.0,
      ny = 0.0,
      dof = Dof 1 1 1,
      globNdx = 1
    }

n4 :: Node
n4 =
  Node
    { nodeId = 4,
      nx = 15.0,
      ny = 0.0,
      dof = Dof 0 0 1,
      globNdx = 5
    }

n5 :: Node
n5 =
  Node
    { nodeId = 5,
      nx = 20.0,
      ny = 0.0,
      dof = Dof 1 1 1,
      globNdx = 6
    }

n21 :: Node
n21 =
  Node
    { nodeId = 6,
      nx = 0.0,
      ny = 0.0,
      dof = Dof 0 0 1,
      globNdx = 4
    }

n22 :: Node
n22 =
  Node
    { nodeId = 7,
      nx = 4.33,
      ny = 2.5,
      dof = Dof 1 1 1,
      globNdx = 9
    }

n23 :: Node
n23 =
  Node
    { nodeId = 9,
      nx = 4.33,
      ny = 2.5,
      dof = Dof 1 1 1,
      globNdx = 12
    }

{- dof4 :: Dof
dof4 = Dof 0 0 1
 -}

dof5 :: Dof
dof5 = Dof 1 0 1

dof6 :: Dof
dof6 = Dof 0 1 1

dof7 :: Dof
dof7 = Dof 1 1 1

spec :: Spec
spec = do
  describe "NodeSpec" $ do
    context "Dof" $ do
      it "1 == Dof 1 0 0" $ do
        let d1 = parseDof "1"
        shouldBe (Dof 1 0 0) d1
        shouldBe 1 (numDof d1)
        shouldBe 1 (bitSum d1)
      it "3 == Dof 1 1 0" $ do
        let d3 = parseDof "3"
        shouldBe (Dof 1 1 0) d3
        shouldBe 2 (numDof d3)
        shouldBe 3 (bitSum d3)
      it "7 == Dof 1 1 1" $ do
        shouldBe (Dof 1 1 1) dof7
        shouldBe 3 (numDof dof7)
        shouldBe 7 (bitSum dof7)
      it "indexSeed dof5" $ do
        shouldBe (indexSeed dof5 True 10) [(0, 10), (2, 11)]
        shouldBe (indexSeed dof5 False 10) [(3, 10), (5, 11)]
      it "indexSeed dof6" $ do
        shouldBe (indexSeed dof6 True 10) [(1, 10), (2, 11)]
        shouldBe (indexSeed dof6 False 10) [(4, 10), (5, 11)]
      it "indexSeed dof7" $ do
        shouldBe (indexSeed dof7 True 10) [(0, 10), (1, 11), (2, 12)]
        shouldBe (indexSeed dof7 False 10) [(3, 10), (4, 11), (5, 12)]
    context "Node indexSeed" $ do
      it "indexSeeds first node n1, second node n2" $
        shouldBe (indexSeeds (FirstNode n1) (SecondNode n2)) [(2, 0), (3, 1), (4, 2), (5, 3)]
      it "indexSeeds first node n2, second node n4" $
        shouldBe (indexSeeds (FirstNode n2) (SecondNode n4)) [(0, 1), (1, 2), (2, 3), (5, 5)]
      it "indexSeeds first node n5, second node n4" $
        shouldBe (indexSeeds (FirstNode n5) (SecondNode n4)) [(0, 6), (1, 7), (2, 8), (5, 5)]
      it "indexSeeds first node n22 second node n23" $
        shouldBe (indexSeeds (FirstNode n22) (SecondNode n23)) [(0, 6), (1, 7), (2, 8), (5, 5)]
    context "Node calcGeom" $ do
      it "Geom first node n21, second node n22" $ do
        let fn = FirstNode n21
        let sn = SecondNode n22
        let geom = calcGeom fn sn
        shouldBe geom (Geom (Cosine 0.86) (Sine 0.5) 5.0)
      it "Geom first node n1, second node n2" $ do
        let fn = FirstNode n1
        let sn = SecondNode n2
        let geom = calcGeom fn sn
        shouldBe geom (Geom (Cosine 1.0) (Sine 0.0) 5.0)
