module StearnsWharf.LoadSpec
  ( spec
  )
where

import Test.Hspec

import StearnsWharf.Load
import StearnsWharf.Node (Dof (..), Node (..))

uls1 :: Load
uls1 = Load 1 0.0 0.0 0.0 (-10.0) 1.4

uls2 :: Load
uls2 = Load 2 0.0 (-10.0) 0.0 (-10.0) 1.4

sls1expected :: Load
sls1expected = Load 1 0.0 0.0 0.0 (-7.14) 1.0

sls2expected :: Load
sls2expected = Load 2 0.0 (-7.14) 0.0 (-7.14) 1.0

lm1expected :: LimitStates Load
lm1expected = LimitStates uls1 sls1expected

lm1actual :: LimitStates Load
lm1actual = limitStates uls1

lm2expected :: LimitStates Load
lm2expected = LimitStates uls2 sls2expected

lm2actual :: LimitStates Load
lm2actual = limitStates uls2

------------------------------------------------------------------

n1 :: Node
n1 =
  Node
    { nodeId = 1
    , nx = 0.0
    , ny = 0.0
    , dof = Dof 0 0 1
    , globNdx = 0
    }

ulp1 :: PointLoad
ulp1 = PointLoad 1 (-12.0) 90.0 1.4 n1

-- ulp2 :: PointLoad
-- ulp2 = PointLoad 1 (-12.0) 90.0 1.4 n1

slp1expected :: PointLoad
slp1expected = PointLoad 1 (-8.57) 90.0 1.0 n1

lmp1expected :: LimitStates PointLoad
lmp1expected = LimitStates ulp1 slp1expected

lmp1actual :: LimitStates PointLoad
lmp1actual = limitStatesPt ulp1

spec :: Spec
spec = do
  describe "LoadSpec" $ do
    context "LimitStates Load" $ do
      it "lm1expected should be equal to lm1actual" $ do
        shouldBe lm1expected lm1actual
      it "lm2expected should be equal to lm2actual" $ do
        shouldBe lm2expected lm2actual
    context "LimitStates PointLoad" $ do
      it "lmp1expected should be equal to lmp1actual" $ do
        shouldBe lmp1expected lmp1actual

{-     context "loads" $ do
      it "loads sls2 and sls3 should be equal" $ do
        shouldBe sls2 sls3
      it "loads sls2 and sls1 should not be equal" $ do
        shouldBe (sls2 == sls1) False
    context "limitStates" $ do
      it "limitStates lm1 and lm3 should be equal" $ do
        shouldBe lm1 lm3
      it "limitStates lm1 and lm2 should not be equal" $ do
        shouldBe (lm1 == lm2) False -}
