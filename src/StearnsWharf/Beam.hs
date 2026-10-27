{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module StearnsWharf.Beam where

-- import Data.Maybe (fromJust)
-- import Data.List (find)

import Control.Monad.ST (ST)
import Numeric.LinearAlgebra (Matrix, Vector, (#>), (<>), (|>))
import qualified Numeric.LinearAlgebra as L
import Numeric.LinearAlgebra.Devel (STMatrix, STVector)
import qualified Numeric.LinearAlgebra.Devel as D
-- import qualified StearnsWharf.Materials as M
-- import StearnsWharf.Common (Cosine(..),(Sine..))

import StearnsWharf.Common
  ( Cosine (..),
    Sine (..),
  )
import StearnsWharf.Load (LimitStates, Load (..))
import qualified StearnsWharf.Load as L
import StearnsWharf.Node
  ( FirstNode (..),
    Geom (..),
    Node (..),
    SecondNode (..),
  )
import qualified StearnsWharf.Node as N
import StearnsWharf.Profile (Profile)
import qualified StearnsWharf.Profile as P
import Prelude hiding ((<>))

-- newtype BeamId = BeamId String deriving (Show)
type BeamId = Int

data BeamProp a = BeamProp
  { beamId :: BeamId,
    n1 :: FirstNode,
    n2 :: SecondNode,
    bt :: a,
    limitStates :: Maybe (LimitStates Load)
  }
  deriving (Show)

data Beam a
  = Bjlk33 (BeamProp a)
  | Bjlk11 (BeamProp a)
  | Bjlk32 (BeamProp a)
  deriving (Show)

instance Eq (BeamProp a) where
  (==) bp1 bp2 =
    ((beamId bp1) == (beamId bp2))
      && ((n1 bp1) == (n1 bp2))
      && ((n2 bp1) == (n2 bp2))
      && ((limitStates bp1) == (limitStates bp2))

instance Ord (BeamProp a) where
  compare bp1 bp2 = compare (beamId bp1) (beamId bp2)

instance Eq (Beam a) where
  (==) (Bjlk33 b1) (Bjlk33 b2) = b1 == b2
  (==) (Bjlk11 b1) (Bjlk11 b2) = b1 == b2
  (==) (Bjlk32 b1) (Bjlk32 b2) = b1 == b2
  (==) _ _ = False

instance Ord (Beam a) where
  compare (Bjlk33 b1) (Bjlk33 b2) = compare b1 b2
  compare (Bjlk11 b1) (Bjlk11 b2) = compare b1 b2
  compare (Bjlk32 b1) (Bjlk32 b2) = compare b1 b2
  compare _ _ = EQ

{-
calcEal :: Profile a => a -> Double -> Double
calcEal bt' len =
  let
    aa = P.area bt'
    ee = P.emodulus bt'
    ii = P.secondAreaMoment bt'
  in
  ee*aa/len
-}

createK :: (Profile a) => Beam a -> Matrix Double
createK (Bjlk33 BeamProp {n1, n2, bt}) =
  let aa = P.area bt
      ee = P.emodulus bt
      ii = P.secondAreaMoment bt
      (Geom _ _ len) = N.calcGeom n1 n2
      eal = ee * aa / len
      b = ee * ii / (len ** 3.0)
      k11 = 12 * b
      k12 = (-6) * b * len
      k14 = (-12) * b
      k15 = k12
      k22 = 4 * b * len ** 2
      k24 = 6 * b * len
      k25 = 2 * b * len ** 2
      k44 = k11 -- 12*b
      k45 = k24 -- 6*b*len
      k55 = k22 -- 4*b*len**2
   in L.fromLists
        [ [eal, 0.0, 0.0, -eal, 0.0, 0.0],
          [0.0, k11, k12, 0.0, k14, k15],
          [0.0, k12, k22, 0.0, k24, k25],
          [-eal, 0.0, 0.0, eal, 0.0, 0.0],
          [0.0, k14, k24, 0.0, k44, k45],
          [0.0, k15, k25, 0.0, k45, k55]
        ]
createK (Bjlk11 BeamProp {n1, n2, bt}) =
  let aa = P.area bt
      ee = P.emodulus bt
      (Geom _ _ len) = N.calcGeom n1 n2
      eal = ee * aa / len
   in L.fromLists
        [ [eal, 0.0, 0.0, -eal, 0.0, 0.0],
          [0.0, 0.0, 0.0, 0.0, 0.0, 0.0],
          [0.0, 0.0, 0.0, 0.0, 0.0, 0.0],
          [-eal, 0.0, 0.0, eal, 0.0, 0.0],
          [0.0, 0.0, 0.0, 0.0, 0.0, 0.0],
          [0.0, 0.0, 0.0, 0.0, 0.0, 0.0]
        ]
createK (Bjlk32 BeamProp {n1, n2, bt}) =
  let aa = P.area bt
      ee = P.emodulus bt
      ii = P.secondAreaMoment bt
      (Geom _ _ len) = N.calcGeom n1 n2
      eal = ee * aa / len
      b = ee * ii / (len ** 3.0)
      k11 = 3 * b
      k24 = 3 * b * len
      k12 = (-k24)
      k14 = (-k11)
      k22 = 3 * b * len * len
      k44 = k11
   in L.fromLists
        [ [eal, 0.0, 0.0, -eal, 0.0, 0.0], -- 0 5
          [0.0, k11, k12, 0.0, k14, 0.0], -- 6  11
          [0.0, k12, k22, 0.0, k24, 0.0], -- 12 17
          [-eal, 0.0, 0.0, eal, 0.0, 0.0], -- 18 23
          [0.0, k14, k24, 0.0, k44, 0.0], -- 14 29
          [0.0, 0.0, 0.0, 0.0, 0.0, 0.0] -- 30 35
        ]

createK_ :: (Profile a) => Beam a -> Matrix Double
createK_ beam =
  let (Geom c s _) = calcGeom beam
      curTg = tg c s
      locK = createK beam
   in L.tr curTg <> locK <> curTg

createSY :: Geom -> Load -> Vector Double
createSY (Geom _ _ len) load =
  let q1 = (L.qz1 load)
      q2 = (L.qz2 load)
      f = len / 60.0
      s0 = 0.0
      s1 = ((-21.0) * q1 - (9.0 * q2)) * f
      s2 = ((3.0 * len * q1) + (2.0 * len * q2)) * f
      s3 = 0.0
      s4 = ((-9.0) * q1 - (21.0 * q2)) * f
      s5 = ((-2.0 * len * q1) - (3.0 * len * q2)) * f
   in L.fromList [s0, s1, s2, s3, s4, s5]

createSY_ :: Geom -> Load -> Vector Double
createSY_ g@(Geom c s _) load =
  let tgm = L.tr $ tg c s
   in tgm #> (createSY g load)

emptySY :: Vector Double
emptySY = L.fromList [0.0, 0.0, 0.0, 0.0, 0.0, 0.0]

calcGeom :: Beam a -> Geom
calcGeom (Bjlk33 beam) = N.calcGeom (n1 beam) (n2 beam)
calcGeom _ = (Geom (Cosine 0.0) (Sine 0.0) 0.0)

tg :: Cosine -> Sine -> Matrix Double
tg (Cosine c) (Sine s) =
  L.fromLists
    [ [c, s, 0, 0, 0, 0],
      [-s, c, 0, 0, 0, 0],
      [0, 0, 1, 0, 0, 0],
      [0, 0, 0, c, s, 0],
      [0, 0, 0, -s, c, 0],
      [0, 0, 0, 0, 0, 1]
    ]

add2systemK :: (Profile a) => STMatrix s Double -> Beam a -> ST s ()
add2systemK m beam =
  undefined

-- let
--   myk = createK_
-- in
--   undefined
