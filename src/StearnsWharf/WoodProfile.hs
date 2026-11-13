{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module StearnsWharf.WoodProfile where

import StearnsWharf.Common (Height (..), Shear (..), StaticMoment (..), Width (..))
import StearnsWharf.Profile (Profile)
import qualified StearnsWharf.Profile as P

-- import StearnsWharf.Material (Material(..),MaterialCategory(..),Mprop(..),StrengthClass(..))
-- import qualified StearnsWharf.Material as M

data StrengthClass
  = C18
  | C24
  | C30
  | C40
  | GL28c
  | GL30c
  | GL32c
  | TEST_CLASS
  deriving (Show)

data MaterialProperties = MaterialProperties
  { emodulus,
    mySigma,
    myTau ::
      Double,
    stClass :: StrengthClass
  }
  deriving (Show)

data WoodCategory
  = Wood
  | Glulam
  deriving (Show)

data Material
  = Material WoodCategory MaterialProperties
  deriving (Show)

data WoodProfile = WoodProfile
  { width :: Width,
    height :: Height,
    matr :: Material
  }
  deriving (Show)

asStrengthClass :: String -> StrengthClass
asStrengthClass scs =
  case scs of
    "C18" -> C18
    "C24" -> C24
    "C30" -> C30
    "C40" -> C40
    "GL28c" -> GL28c
    "GL30c" -> GL30c
    "GL32c" -> GL32c
    _ -> undefined

createWoodProfile ::
  StrengthClass ->
  Width ->
  Height ->
  WoodProfile
createWoodProfile stc w h = case stc of
  C18 -> WoodProfile w h (Material Wood (MaterialProperties 9000 18 3.4 stc))
  C24 -> WoodProfile w h (Material Wood (MaterialProperties 11000 24 4.0 stc))
  C30 -> WoodProfile w h (Material Wood (MaterialProperties 12000 30 4.0 stc))
  C40 -> WoodProfile w h (Material Wood (MaterialProperties 14000 40 4.0 stc))
  GL28c -> WoodProfile w h (Material Glulam (MaterialProperties 12500 28 3.5 stc))
  GL30c -> WoodProfile w h (Material Glulam (MaterialProperties 13000 30 3.5 stc))
  GL32c -> WoodProfile w h (Material Glulam (MaterialProperties 13500 32 3.5 stc))
  TEST_CLASS -> WoodProfile w h (Material Wood (MaterialProperties 1000 10 1.0 stc))

instance Profile WoodProfile where
  desc WoodProfile {matr = (Material wcat mp)} =
    "Wood Profile: " ++ (show wcat) ++ ", " ++ (show $ mp.stClass)
  sigma wp (StaticMoment moment) = moment / (1000.0 * (P.sectionModulus wp))
  tau wp (Shear shr) = (3.0 * shr) / (2000.0 * (P.area wp))
  area WoodProfile {width = Width w, height = Height h} = (w * h) / 1000000.0
  emodulus WoodProfile {matr = (Material _ mp)} = mp.emodulus * 1000.0
  secondAreaMoment WoodProfile {width = Width w, height = Height h} = ((w / 1000) * ((h / 1000) ** 3)) / 12.0
  sectionModulus WoodProfile {width = Width w, height = Height h} = ((w / 1000) * ((h / 1000) ** 2)) / 6.0
  centroid WoodProfile {height = Height h} = h / 2000.0

{-
desc _ = "Wood Profile"
sigma          _ _ = 0.0
tau            _ _ = 0.0
area           _ = 0.0
emodulus       _ = 0.0
secondAreaMoment _ = 0.0
sectionModulus   _ = 0.0
centroid         _ = 0.0
-}
