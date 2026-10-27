{-# LANGUAGE NamedFieldPuns #-}

module StearnsWharf.BeamSpec
  ( spec,
  )
where

import Numeric.LinearAlgebra
  ( Vector,
    flatten,
    fromList,
    toList,
  )
import Numeric.LinearAlgebra.Devel
  ( at',
  )
import StearnsWharf.Beam
import StearnsWharf.Common
  ( Height (..),
    Width (..),
  )
import StearnsWharf.Load
import StearnsWharf.Node hiding (calcGeom)
import qualified StearnsWharf.Profile as P
import StearnsWharf.WoodProfile
import Test.Hspec

diffLists :: [Double] -> [Double] -> [Double]
diffLists [] _ = []
diffLists _ [] = []
diffLists (x : xs) (y : ys) = abs (x - y) : diffLists xs ys

eqVec :: Vector Double -> Vector Double -> Bool
eqVec v1 v2 =
  let v1' = toList v1
      v2' = toList v2
   in if length v1' /= length v2'
        then False
        else
          let diffResult = diffLists v1' v2'
           in all (\x -> x < 0.011) diffResult

no1 :: Node
no1 =
  Node
    { nodeId = 1,
      nx = 0.0,
      ny = 0.0,
      dof = Dof 0 0 1,
      globNdx = 0
    }

no12 :: Node
no12 =
  Node
    { nodeId = 2,
      nx = 10.0,
      ny = 0.0,
      dof = Dof 0 0 1,
      globNdx = 1
    }

no13 :: Node
no13 =
  Node
    { nodeId = 3,
      nx = 0.0,
      ny = 10.0,
      dof = Dof 0 0 1,
      globNdx = 1
    }

{- no14 :: Node
no14 =
  Node
  { nodeId = 4
  , nx = 10.0
  , ny = 0.0
  , dof = Dof 0 0 1
  , globNdx = 1
  }
 -}

no2 :: Node
no2 =
  Node
    { nodeId = 5,
      nx = 4.33,
      ny = 2.5,
      dof = Dof 1 1 1,
      globNdx = 1
    }

{- no3 :: Node
no3 =
  Node
  { nodeId = 6
  , nx = 8.66
  , ny = 5.0
  , dof = Dof 0 0 1
  , globNdx = 4
  } -}

tv1 :: Vector Double
tv1 = fromList [2.3, 3.0, 0.0, (-4.0), 3.2, 0.0]

tv2 :: Vector Double
tv2 = fromList [2.3, 3.0, 0.0, (-4.0), 3.2, 0.0]

tv3 :: Vector Double
tv3 = fromList [2.4, 3.0, 0.0, (-4.0), 3.2, 0.0]

tv4 :: Vector Double
tv4 = fromList []

load1 :: Load
load1 = (Load 1 0.0 (-10.0) 0.0 (-10.0) 1.5)

load2 :: Load
load2 = (Load 2 0.0 (-10.0) 0.0 0.0 1.5)

c24 :: WoodProfile
c24 = createWoodProfile TEST_CLASS (Width 100) (Height 200)

beam1 :: Beam WoodProfile
beam1 =
  let bid = 1
      fn = (FirstNode no1)
      sn = (SecondNode no12)
      load = Nothing
      profile = c24
      bp = BeamProp bid fn sn profile load
   in Bjlk33 bp

beam2 :: Beam WoodProfile
beam2 =
  let bid = 2
      fn = (FirstNode no1)
      sn = (SecondNode no13)
      load = Nothing
      profile = c24
      bp = BeamProp bid fn sn profile load
   in Bjlk33 bp

beam3 :: Beam WoodProfile
beam3 =
  let bid = 3
      fn = (FirstNode no1)
      sn = (SecondNode no2)
      load = Nothing
      profile = c24
      bp = BeamProp bid fn sn profile load
   in Bjlk33 bp

beam4 :: Beam WoodProfile
beam4 =
  let bid = 2
      fn = (FirstNode no1)
      sn = (SecondNode no13)
      load = Nothing
      profile = c24
      bp = BeamProp bid fn sn profile load
   in Bjlk33 bp

sy1 :: Vector Double
sy1 = fromList [0.0, 50.0, -83.33, 0.0, 50.0, 83.33]

sy2 :: Vector Double
sy2 = fromList [0.0, 35.0, -50.0, 0.0, 15.0, 33.33]

sy3 :: Vector Double
sy3 = fromList [-50.0, 0.0, -83.33, -50.0, 0.0, 83.33]

sy4 :: Vector Double
sy4 = fromList [-12.50, 21.65, -20.83, -12.50, 21.65, 20.83]

bk1 :: Vector Double
bk1 =
  fromList
    [ 2000.0,
      0.0,
      0.0,
      -2000.0,
      0.0,
      0.0,
      0.0,
      0.8000000000000002,
      -4.000000000000001,
      0.0,
      -0.8000000000000002,
      -4.000000000000001,
      0.0,
      -4.000000000000001,
      26.66666666666667,
      0.0,
      4.000000000000001,
      13.333333333333336,
      -2000.0,
      0.0,
      0.0,
      2000.0,
      0.0,
      0.0,
      0.0,
      -0.8000000000000002,
      4.000000000000001,
      0.0,
      0.8000000000000002,
      4.000000000000001,
      0.0,
      -4.000000000000001,
      13.333333333333336,
      0.0,
      4.000000000000001,
      26.66666666666667
    ]

{-
beam4 :: Beam WoodProfile
beam4 =
  let
    bid = BeamId "beam4"
    fn = (FirstNode no1)
    sn = (SecondNode no14)
    load = Nothing
    profile = c24
  in
  (Bjlk32 bid fn sn profile load)

expectedBjlk32 :: Vector Double
expectedBjlk32 =
  fromList
   [0.0,0.0,0.0,-0.0,0.0,0.0,0.0,0.0,-0.0,0.0,-0.0,-0.0,0.0,-0.0,
   0.0,0.0,0.0,0.0,-0.0,0.0,0.0,0.0,0.0,0.0,0.0,-0.0,0.0,0.0,0.0,
   0.0,0.0,-0.0,0.0,0.0,0.0,0.0]
-}

showBeamK :: Vector Double -> Beam WoodProfile -> IO ()
showBeamK v (Bjlk33 BeamProp {bt}) =
  let k00 = at' v 0
      k01 = at' v 1
      k02 = at' v 2
      k03 = at' v 3
      k04 = at' v 4
      k05 = at' v 5

      k11 = at' v 7
      k12 = at' v 8
      k13 = at' v 9
      k14 = at' v 10
      k15 = at' v 11

      k22 = at' v 14
      k23 = at' v 15
      k24 = at' v 16
      k25 = at' v 17

      k33 = at' v 21
      k34 = at' v 22
      k35 = at' v 23

      k44 = at' v 28
      k45 = at' v 29

      k55 = at' v 35
      aa = P.area bt
      ee = P.emodulus bt
      ii = P.secondAreaMoment bt
   in putStrLn ("k00: " <> show k00)
        >> putStrLn ("k01: " <> show k01)
        >> putStrLn ("k02: " <> show k02)
        >> putStrLn ("k03: " <> show k03)
        >> putStrLn ("k04: " <> show k04)
        >> putStrLn ("k05: " <> show k05)
        >> putStrLn ("k11: " <> show k11)
        >> putStrLn ("k12: " <> show k12)
        >> putStrLn ("k13: " <> show k13)
        >> putStrLn ("k14: " <> show k14)
        >> putStrLn ("k15: " <> show k15)
        >> putStrLn ("k22: " <> show k22)
        >> putStrLn ("k23: " <> show k23)
        >> putStrLn ("k24: " <> show k24)
        >> putStrLn ("k25: " <> show k25)
        >> putStrLn ("k33: " <> show k33)
        >> putStrLn ("k34: " <> show k34)
        >> putStrLn ("k35: " <> show k35)
        >> putStrLn ("k44: " <> show k44)
        >> putStrLn ("k44: " <> show k45)
        >> putStrLn ("k55: " <> show k55)
        >> putStrLn ("E-modulus: " <> show ee)
        >> putStrLn ("Area: " <> show aa)
        >> putStrLn ("I: " <> show ii)

spec :: Spec
spec = do
  describe "BeamSpec" $ do
    context "eqVec" $ do
      it "tv1 and tv2 equal" $ do
        shouldBe (eqVec tv1 tv2) True
      it "tv1 and tv3 not equal" $ do
        shouldBe (eqVec tv1 tv3) False
      it "tv1 and tv4 not equal" $ do
        shouldBe (eqVec tv1 tv4) False
    context "createSY" $ do
      let geom1 = calcGeom beam1
      it "local coordinates" $ do
        let syResult = createSY geom1 load1
        shouldBe (eqVec syResult sy1) True
        let syResult2 = createSY geom1 load2
        shouldBe (eqVec syResult2 sy2) True
      it "global coordinates beam1" $ do
        let syResult_ = createSY_ geom1 load1
        shouldBe (eqVec syResult_ sy1) True
      it "global coordinates beam2" $ do
        let geom2 = calcGeom beam2
        let syResult2_ = createSY_ geom2 load1
        shouldBe (eqVec syResult2_ sy3) True
      it "global coordinates beam3" $ do
        let geom3 = calcGeom beam3
        let syResult3_ = createSY_ geom3 load1
        shouldBe (eqVec syResult3_ sy4) True
      it "beam2 should not be equal to beam3" $ do
        shouldBe (beam2 == beam3) False
      it "beam2 should be equal to beam4" $ do
        shouldBe beam2 beam4
    context "createK" $ do
      it "createK_ beam1 should be equal to bk1" $ do
        let actual = flatten $ createK_ beam1
        -- showBeamK actual beam1
        -- shouldBe bk1 actual
        shouldBe (eqVec actual bk1) True
