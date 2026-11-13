module StearnsWharf.Steel.HatProfilesSpec
  ( spec,
  )
where

import StearnsWharf.Common
  ( floatEq2,
  )
import qualified StearnsWharf.Profile as P
import StearnsWharf.Steel.HatProfiles
import qualified StearnsWharf.Steel.Plates as PL
import Test.Hspec

spec :: Spec
spec = do
  describe "HatProfilesSpec" $ do
    context "TEHP 200 10 200 20 150 15" $ do
      it "Dummy" $ do
        shouldBe 1 1

-- let t = createTEHP 200 10 100 20 150 15
-- it "Area of TEHP" $ do
--   shouldBe (floatEq2 (P.area t) 0.00785 0.001) True
-- it "Web height of TEHP" $ do
--   shouldBe (webHeight t) 0.18
-- it "Height of top" $ do
--   shouldBe (PL.h (topPl t)) 20.0
-- it "Total height of TEHP" $ do
--   shouldBe (totalHeight t) 0.215
-- it "Centroid of TEHP" $ do
--   shouldBe (P.centroid t) 0.4
