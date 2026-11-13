{-# LANGUAGE RecordWildCards #-}

module StearnsWharf.Transform.YamlTransformSpec
  ( spec,
  )
where

import Data.List (sort)
-- import StearnsWharf.Profile (Profile)

import qualified StearnsWharf.Beam as B
import StearnsWharf.Common (Height (..), Width (..))
import qualified StearnsWharf.Load as L
import qualified StearnsWharf.Node as N
import qualified StearnsWharf.System as S
import StearnsWharf.Transform.YamlTransform
import qualified StearnsWharf.WoodProfile as W
import Test.Hspec

{- nodeEq :: (N.Node, N.Node) -> Bool
nodeEq (n1, n2) =
  all ((==) True)
  [ N.nodeId n1 == N.nodeId n2
  , N.nx n1 == N.nx n2
  , N.ny n1 == N.ny n2
  , N.dof n1 == N.dof n2
  , N.globNdx n1 == N.globNdx n2
  ]

loadEq :: (L.Load, L.Load) -> Bool
loadEq (l1, l2) =
  all ((==) True)
  [ L.loadId l1 == L.loadId l2
  , L.qx1 l1 == L.qx1 l2
  , L.qz1 l1 == L.qz1 l2
  , L.qx2 l1 == L.qx2 l2
  , L.qz2 l1 == L.qz2 l2
  , L.loadFactor l1 == L.loadFactor l2
  ]

pointLoadEq :: (L.PointLoad, L.PointLoad) -> Bool
pointLoadEq (l1, l2) =
  all ((==) True)
  [ L.ploadId l1 == L.ploadId l2
  , L.plVal l1 == L.plVal l2
  , L.plAngle l1 == L.plAngle l2
  , L.plFactor l1 == L.plFactor l2
  , L.node l1 == L.node l2
  ]
 -}

testSystemYaml :: YamlSystem
testSystemYaml =
  YamlSystem
    { nodes =
        [ YamlNode {nid = 1, x = 0.0, y = 0.0, dof = 1},
          YamlNode {nid = 2, x = 1.75, y = 0.0, dof = 7},
          YamlNode {nid = 3, x = 3.5, y = 0.0, dof = 1},
          YamlNode {nid = 4, x = 7.0, y = 0.0, dof = 7},
          YamlNode {nid = 5, x = 10.0, y = 0.0, dof = 1}
        ],
      loads =
        [ YamlLoad {lid = 1, f = 1.4, ly1 = -10.0, ly2 = -10.0},
          YamlLoad {lid = 2, f = 1.4, ly1 = 0.0, ly2 = -23.0}
        ],
      pointloads =
        [ YamlPointLoad {pid = 1, pf = 1.4, v = -55.0, ang = 90, pnode = 2}
        ],
      woodprofiles =
        [ YamlWoodProfile
            { stclass = "GL32c",
              profiles =
                [ WoodProfileInternal
                    { w = 90.0,
                      h = 270.0,
                      b33 =
                        [ B33 {bid = 1, ld = 1, n1 = 1, n2 = 2},
                          B33 {bid = 2, ld = 2, n1 = 2, n2 = 3}
                        ]
                    },
                  WoodProfileInternal
                    { w = 115.0,
                      h = 270.0,
                      b33 =
                        [ B33 {bid = 3, ld = 1, n1 = 3, n2 = 4},
                          B33 {bid = 4, ld = 0, n1 = 4, n2 = 5}
                        ]
                    }
                ]
            }
        ]
    }

woodProfilesSystem :: [B.Beam W.WoodProfile]
woodProfilesSystem =
  [ B.Bjlk33
      ( B.BeamProp
          { beamId = 1,
            n1 = N.FirstNode (N.Node {nodeId = 1, nx = 0.0, ny = 0.0, dof = N.Dof {dofX = 1, dofY = 0, dofM = 0}, globNdx = 0}),
            n2 = N.SecondNode (N.Node {nodeId = 2, nx = 1.75, ny = 0.0, dof = N.Dof {dofX = 1, dofY = 1, dofM = 1}, globNdx = 1}),
            bt =
              W.WoodProfile
                { width = Width 90.0,
                  height = Height 270.0,
                  matr = W.Material W.Glulam (W.MaterialProperties {emodulus = 13500.0, mySigma = 32.0, myTau = 3.5, stClass = W.GL32c})
                },
            limitStates =
              Just
                ( L.LimitStates
                    { uls = L.Load {loadId = 1, qx1 = 0.0, qz1 = -10.0, qx2 = 0.0, qz2 = -10.0, loadFactor = 1.4},
                      sls = L.Load {loadId = 1, qx1 = 0.0, qz1 = -7.142857142857143, qx2 = 0.0, qz2 = -7.142857142857143, loadFactor = 1.0}
                    }
                )
          }
      ),
    B.Bjlk33
      ( B.BeamProp
          { beamId = 2,
            n1 = N.FirstNode (N.Node {nodeId = 2, nx = 1.75, ny = 0.0, dof = N.Dof {dofX = 1, dofY = 1, dofM = 1}, globNdx = 1}),
            n2 = N.SecondNode (N.Node {nodeId = 3, nx = 3.5, ny = 0.0, dof = N.Dof {dofX = 1, dofY = 0, dofM = 0}, globNdx = 4}),
            bt =
              W.WoodProfile
                { width = Width 90.0,
                  height = Height 270.0,
                  matr =
                    W.Material
                      W.Glulam
                      (W.MaterialProperties {emodulus = 13500.0, mySigma = 32.0, myTau = 3.5, stClass = W.GL32c})
                },
            limitStates =
              Just
                ( L.LimitStates
                    { uls = L.Load {loadId = 2, qx1 = 0.0, qz1 = 0.0, qx2 = 0.0, qz2 = -23.0, loadFactor = 1.4},
                      sls = L.Load {loadId = 2, qx1 = 0.0, qz1 = 0.0, qx2 = 0.0, qz2 = -16.42857142857143, loadFactor = 1.0}
                    }
                )
          }
      ),
    B.Bjlk33
      ( B.BeamProp
          { beamId = 3,
            n1 = N.FirstNode (N.Node {nodeId = 3, nx = 3.5, ny = 0.0, dof = N.Dof {dofX = 1, dofY = 0, dofM = 0}, globNdx = 4}),
            n2 = N.SecondNode (N.Node {nodeId = 4, nx = 7.0, ny = 0.0, dof = N.Dof {dofX = 1, dofY = 1, dofM = 1}, globNdx = 5}),
            bt =
              W.WoodProfile
                { width = Width 115.0,
                  height = Height 270.0,
                  matr = W.Material W.Glulam (W.MaterialProperties {emodulus = 13500.0, mySigma = 32.0, myTau = 3.5, stClass = W.GL32c})
                },
            limitStates =
              Just
                ( L.LimitStates
                    { uls = L.Load {loadId = 1, qx1 = 0.0, qz1 = -10.0, qx2 = 0.0, qz2 = -10.0, loadFactor = 1.4},
                      sls = L.Load {loadId = 1, qx1 = 0.0, qz1 = -7.142857142857143, qx2 = 0.0, qz2 = -7.142857142857143, loadFactor = 1.0}
                    }
                )
          }
      ),
    B.Bjlk33
      ( B.BeamProp
          { beamId = 4,
            n1 = N.FirstNode (N.Node {nodeId = 4, nx = 7.0, ny = 0.0, dof = N.Dof {dofX = 1, dofY = 1, dofM = 1}, globNdx = 5}),
            n2 = N.SecondNode (N.Node {nodeId = 5, nx = 10.0, ny = 0.0, dof = N.Dof {dofX = 1, dofY = 0, dofM = 0}, globNdx = 8}),
            bt =
              W.WoodProfile
                { width = Width 115.0,
                  height = Height 270.0,
                  matr = W.Material W.Glulam (W.MaterialProperties {emodulus = 13500.0, mySigma = 32.0, myTau = 3.5, stClass = W.GL32c})
                },
            limitStates = Nothing
          }
      )
  ]

testSystem :: S.System
testSystem =
  S.System
    { nodes =
        [ N.Node {nodeId = 1, nx = 0.0, ny = 0.0, dof = N.Dof {dofX = 1, dofY = 0, dofM = 0}, globNdx = 0},
          N.Node {nodeId = 2, nx = 1.75, ny = 0.0, dof = N.Dof {dofX = 1, dofY = 1, dofM = 1}, globNdx = 1},
          N.Node {nodeId = 3, nx = 3.5, ny = 0.0, dof = N.Dof {dofX = 1, dofY = 0, dofM = 0}, globNdx = 4},
          N.Node {nodeId = 4, nx = 7.0, ny = 0.0, dof = N.Dof {dofX = 1, dofY = 1, dofM = 1}, globNdx = 5},
          N.Node {nodeId = 5, nx = 10.0, ny = 0.0, dof = N.Dof {dofX = 1, dofY = 0, dofM = 0}, globNdx = 8}
        ],
      loads =
        [ L.Load {loadId = 1, qx1 = 0.0, qz1 = -10.0, qx2 = 0.0, qz2 = -10.0, loadFactor = 1.4},
          L.Load {loadId = 2, qx1 = 0.0, qz1 = 0.0, qx2 = 0.0, qz2 = -23.0, loadFactor = 1.4}
        ],
      pointLoads =
        [ L.PointLoad
            { ploadId = 1,
              plVal = -55.0,
              plAngle = 90.0,
              plFactor = 1.4,
              node =
                N.Node
                  { nodeId = 2,
                    nx = 1.75,
                    ny = 0.0,
                    dof = N.Dof {dofX = 1, dofY = 1, dofM = 1},
                    globNdx = 1
                  }
            }
        ],
      woodProfiles = woodProfilesSystem
    }

{-
equalNodes :: [N.Node] -> [N.Node] -> Bool
equalNodes expected actual =
  all ((==) True) $ map nodeEq $ zip expected actual

equalLoads :: [L.Load] -> [L.Load] -> Bool
equalLoads expected actual =
  all ((==) True) $ map loadEq $ zip expected actual

equalPointLoads :: [L.PointLoad] -> [L.PointLoad] -> Bool
equalPointLoads expected actual =
  all ((==) True) $ map pointLoadEq $ zip expected actual

equalWoodProfiles :: [B.Beam a] -> [B.Beam a] -> Bool
equalWoodProfiles expected actual =
  all ((==) True) $ map (\(b1,b2) -> b1 == b2) $ zip expected actual

equalSystem :: S.System -> S.System -> Bool
equalSystem expected actual =
  all (\x -> x == True)
  [ equalNodes (S.nodes expected) (S.nodes actual)
  , equalLoads (S.loads expected) (S.loads actual)
  ] -}

spec :: Spec
spec = do
  describe "YamlTransformSpec" $ do
    context "transformYamlToSystem" $ do
      let actualSystem = transformYamlToSystem testSystemYaml
      it "transformNodes" $ do
        let actual = sort actualSystem.nodes
        shouldBe testSystem.nodes actual
      it "transformLoads" $ do
        let actual = sort actualSystem.loads
        shouldBe testSystem.loads actual
      it "transformPointLoads" $ do
        let actual = sort actualSystem.pointLoads
        shouldBe testSystem.pointLoads actual
      it "transformWoodProfiles" $ do
        let actual = sort actualSystem.woodProfiles
        shouldBe testSystem.woodProfiles actual
