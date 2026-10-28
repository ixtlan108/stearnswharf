{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module StearnsWharf.Demo where

import Numeric.LinearAlgebra (Matrix, Vector, (#>), (<>), (|>))
import StearnsWharf.Beam
import StearnsWharf.Common
  ( Height (..),
    Width (..),
  )
import StearnsWharf.Load (Load (..))
import qualified StearnsWharf.Load as L
import StearnsWharf.Node hiding (calcGeom)
import qualified StearnsWharf.Node as N
import StearnsWharf.Profile (Profile (..))
import qualified StearnsWharf.Profile as P
import StearnsWharf.WoodProfile

c24 :: WoodProfile
c24 = createWoodProfile TEST_CLASS (Width 100) (Height 200)

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
      ny = 10.0,
      dof = Dof 0 0 1,
      globNdx = 1
    }

no13 :: Node
no13 =
  Node
    { nodeId = 2,
      nx = 10.0,
      ny = 0.0,
      dof = Dof 0 0 1,
      globNdx = 1
    }

uls1 :: Load
uls1 = Load 1 0.0 (-10.0) 0.0 (-10.0) 1.4

uls2 :: Load
uls2 = Load 2 0.0 (-10.0) 0.0 (-10.0) 1.4

beam1 :: Beam WoodProfile
beam1 =
  let bid = 1
      fn = (FirstNode no1)
      sn = (SecondNode no13)
      load = Just $ L.limitStates uls1
      profile = c24
      bp = BeamProp bid fn sn profile load
   in Bjlk33 bp

beam2 :: Beam WoodProfile
beam2 =
  let bid = 1
      fn = (FirstNode no1)
      sn = (SecondNode no12)
      load = Just $ L.limitStates uls1
      profile = c24
      bp = BeamProp bid fn sn profile load
   in Bjlk11 bp

beam3 :: Beam WoodProfile
beam3 =
  let bid = 1
      fn = (FirstNode no1)
      sn = (SecondNode no13)
      load = Just $ L.limitStates uls1
      profile = c24
      bp = BeamProp bid fn sn profile load
   in Bjlk32 bp

bjlk1 = createK_ beam1

bjlk2 = createK_ beam2

bjlk3 = createK_ beam3

geom = calcGeom beam1

sy = createSY_ geom uls1

myTg :: (Profile a) => Beam a -> Matrix Double
myTg beam =
  let (Geom c s _) = calcGeom beam
   in tg c s

area1 :: (Profile a) => Beam a -> Double
area1 (Bjlk11 BeamProp {n1, n2, bt}) =
  let aa = P.area bt
      ee = P.emodulus bt
      (Geom _ _ len) = N.calcGeom n1 n2
   in ee * aa / len
area1 _ = 0.0

-- x :: Int
-- x = 3
--
-- test :: Num a => a
-- test = 42
--
-- data Load = Load Int deriving (Show)
-- data PointLoad = PointLoad Int deriving (Show)
--
-- instance Eq Load where
--   (==) (Load l1) (Load l2) = l1 == l2
--
-- data Load2
--   = Load2 Int
--   | PointLoad2 Int
--   deriving (Show)
--
-- data LimitStates a = LimitStates
--   { uls :: a
--   , sls :: a
--   }
--   deriving (Show)
--
-- class Clonable a where
--   asSls :: a -> Load2
--
-- instance Clonable Load2 where
--   asSls (Load2 v) = Load2 (v * 2)
--   asSls (PointLoad2 v) = PointLoad2 (v * 10)
--
-- instance (Eq a) => Eq (LimitStates a) where
--   (==) ls1 ls2 =
--     (uls ls1) == (uls ls2)
--       && (sls ls1) == (sls ls2)
--
-- dax :: LimitStates Load
-- dax = LimitStates (Load 3) (Load 45)
--
-- dax2 :: LimitStates Load
-- dax2 = LimitStates (Load 33) (Load 45)
--
-- yax :: Load2 -> LimitStates Load2
-- yax u =
--   LimitStates u (asSls u)
--
-- data TypeA = TypeA1 | TypeA2 deriving (Show)
--
-- class MyClass a where
--   foo :: Int -> a
--
-- instance MyClass TypeA where
--   foo 1 = TypeA1
--   foo _ = TypeA2
--
-- bar :: (MyClass a) => Int -> a
-- bar = foo

{-
import GHC.Generics
import Data.Yaml
import qualified Data.Aeson                    as Aeson

data Node =
  Node
  { nid :: Int
  , x :: Double
  , y :: Double
  , dof :: Int
  } deriving (Show,Eq,Generic)

data Load =
  Load
  { lid :: Int
  , f :: Double
  , lx :: Double
  , ly :: Double
  } deriving (Show,Eq,Generic)

data PointLoad =
  PointLoad
  { pid :: Int
  , pf :: Double
  , v  :: Double
  , ang :: Int
  , pnode :: Int
  } deriving (Show,Eq,Generic)

data B33 =
  B33
  { bid :: Int
  , ld :: Int
  , n1 :: Int
  , n2 :: Int
  } deriving (Show,Eq,Generic)

data WoodProfileInternal =
  WoodProfileInternal
  { w :: Double
  , h :: Double
  , b33 :: [B33]
  } deriving (Show,Eq,Generic)

data WoodProfile =
  WoodProfile
  { stclass :: String
  , profiles :: [WoodProfileInternal]
  } deriving (Show,Eq,Generic)

data System =
  System
  { nodes :: [Node]
  , loads :: [Load]
  , pointloads :: [PointLoad]
  , woodprofiles :: [WoodProfile]
  } deriving (Show,Eq,Generic)

instance FromJSON B33
instance FromJSON WoodProfileInternal
instance FromJSON WoodProfile
instance FromJSON PointLoad
instance FromJSON Load
instance FromJSON Node
instance FromJSON System

demo :: IO (Either ParseException System)
demo =
  decodeFileEither "/home/rcs/opt/haskell/stearnswharf/tmp/demo1.yaml"
-}

{-
instance FromJSON System where
  parseJSON = Aeson.withObject "System" $ \o -> System
    <$> o .: "nodes"
-}

--    <*> o .: "x2"

{-
instance ToJSON System where
  toJSON System {..} = Aeson.object
    [ "x1" .= x1
    , "x2" .= x2
    ]
  toEncoding (System x1 x2) = Aeson.pairs
    ( "x1"
    .= x1
    <> "x2"
    .= x2
    )
-}
