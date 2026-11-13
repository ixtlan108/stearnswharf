{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module StearnsWharf.Load where

-- newtype LoadId = LoadId String deriving (Show)

import StearnsWharf.Common
  ( floatEq,
  )
import StearnsWharf.Node (Node (..))

type LoadId = Int

data PointLoad = PointLoad
  { ploadId :: LoadId,
    plVal :: Double,
    plAngle :: Double,
    plFactor :: Double,
    node :: Node
  }
  deriving (Show)

instance Eq PointLoad where
  (==) l1 l2 =
    l1.ploadId == l2.ploadId
      && floatEq l1.plVal l2.plVal
      && floatEq l1.plAngle l2.plAngle
      && floatEq l1.plFactor l2.plFactor
      && l1.node == l2.node

instance Ord PointLoad where
  compare l1 l2 = compare l1.ploadId l2.ploadId

data Load = Load
  { loadId :: LoadId,
    qx1 :: Double,
    qz1 :: Double,
    qx2 :: Double,
    qz2 :: Double,
    loadFactor :: Double
  }
  deriving (Show)

instance Eq Load where
  (==) l1 l2 =
    l1.loadId == l2.loadId
      && floatEq l1.qx1 l2.qx1
      && floatEq l1.qz1 l2.qz1
      && floatEq l1.qx2 l2.qx2
      && floatEq l1.qz2 l2.qz2
      && floatEq l1.loadFactor l2.loadFactor

instance Ord Load where
  compare l1 l2 = compare l1.loadId l2.loadId

{-
data T a where
    D1 :: Int -> T String
    D2 :: T Bool
    D3 :: (a,a) -> T [a]
 -}

data LimitStates a = LimitStates
  { uls :: a,
    sls :: a
  }
  deriving (Show)

instance (Eq a) => Eq (LimitStates a) where
  (==) ls1 ls2 =
    ls1.uls == ls2.uls
      && ls1.sls == ls2.sls

limitStates :: Load -> LimitStates Load
limitStates ul@Load {..} =
  let sl = Load loadId (qx1 / loadFactor) (qz1 / loadFactor) (qx2 / loadFactor) (qz2 / loadFactor) 1.0
   in LimitStates ul sl

limitStatesPt :: PointLoad -> LimitStates PointLoad
limitStatesPt ul@PointLoad {plVal, plFactor} =
  let plVal' = plVal / plFactor
      sl = ul {plVal = plVal', plFactor = 1.0}
   in LimitStates ul sl

{-   let
    sl =  Load loadId  (qx1 / loadFactor) (qz1 / loadFactor) (qx2 / loadFactor) (qz2 / loadFactor) 1.0
  in
  LimitStates ul sl  -}
