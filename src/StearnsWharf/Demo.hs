module StearnsWharf.Demo where

-- import Numeric.LinearAlgebra ((#>),Matrix,Vector,inv)
-- import Numeric.LinearAlgebra.Devel (runSTMatrix,runSTVector,newVector,newMatrix)
--
import Numeric.LinearAlgebra (Matrix, Vector, (#>), (<>), (|>))
import Numeric.LinearAlgebra.Devel (newMatrix, runSTMatrix)
import StearnsWharf.Beam
import StearnsWharf.Beam qualified as B
import StearnsWharf.Common
  ( Height (..),
    Width (..),
  )
import StearnsWharf.Load (Load (..))
import StearnsWharf.Load qualified as L
import StearnsWharf.Node hiding (calcGeom)
import StearnsWharf.Node qualified as N
import StearnsWharf.Profile (Profile (..))
import StearnsWharf.Profile qualified as P
import StearnsWharf.WoodProfile

data User = User
  { id :: Int,
    name :: String
  }
  deriving (Show)

data User2 = User2
  { id :: Int,
    name :: String
  }
  deriving (Show)

alice = User {id = 1, name = "Alice"}

alice2 = User2 {id = 11, name = "Alice 2"}

alices = [User 1 "1", User 2 "2", User 3 "3"]

usr1 :: User -> String
usr1 u = u.name

ax :: [String]
ax = map usr1 alices

dx = putStrLn alice.name

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

systemK :: Matrix Double
systemK = runSTMatrix $ do
  m <- newMatrix 0.0 10 10
  pure m

system :: IO ()
system = do
  let m = systemK
  let seeds = B.indexSeeds beam1
  putStrLn (show seeds)

bjlk1 = createK_ beam1

bjlk2 = createK_ beam2

bjlk3 = createK_ beam3

geom = calcGeom beam1

geom2 = calcGeom beam2

geom3 = calcGeom beam3

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
