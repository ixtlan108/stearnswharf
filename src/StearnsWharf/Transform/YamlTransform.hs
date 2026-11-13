{-# LANGUAGE RecordWildCards #-}

module StearnsWharf.Transform.YamlTransform where

-- import Control.Monad.State (State,runState,get,put)

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader
  ( Reader,
    ask,
    runReader,
  )
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
import Data.Yaml
  ( FromJSON (..),
    ParseException,
    decodeFileEither,
  )
import GHC.Generics (Generic)
import StearnsWharf.Beam (Beam (..), BeamProp (..))
import StearnsWharf.Common (Height (..), Width (..))
import StearnsWharf.Load (Load (..), PointLoad (..))
import StearnsWharf.Load qualified as L
import StearnsWharf.Node (FirstNode (..), Node (..), SecondNode (..))
import StearnsWharf.Node qualified as N
import StearnsWharf.System (System (..))
import StearnsWharf.System qualified as S
import StearnsWharf.WoodProfile (WoodProfile (..))
import StearnsWharf.WoodProfile qualified as W

data YamlNode = YamlNode
  { nid :: Int,
    x :: Double,
    y :: Double,
    dof :: Int
  }
  deriving (Show, Eq, Generic)

data YamlLoad = YamlLoad
  { lid :: Int,
    f :: Double,
    ly1 :: Double,
    ly2 :: Double
  }
  deriving (Show, Eq, Generic)

data YamlPointLoad = YamlPointLoad
  { pid :: Int,
    pf :: Double,
    v :: Double,
    ang :: Double,
    pnode :: Int
  }
  deriving (Show, Eq, Generic)

data B33 = B33
  { bid :: Int,
    n1 :: Int,
    n2 :: Int,
    ld :: Int
  }
  deriving (Show, Eq, Generic)

data WoodProfileInternal = WoodProfileInternal
  { w :: Double,
    h :: Double,
    b33 :: [B33]
  }
  deriving (Show, Eq, Generic)

data YamlWoodProfile = YamlWoodProfile
  { stclass :: String,
    profiles :: [WoodProfileInternal]
  }
  deriving (Show, Eq, Generic)

data YamlSystem = YamlSystem
  { nodes :: [YamlNode],
    loads :: [YamlLoad],
    pointloads :: [YamlPointLoad],
    woodprofiles :: [YamlWoodProfile]
  }
  deriving (Show, Eq, Generic)

type NodeMap = Map.Map Int Node

type LoadMap = Map.Map Int Load

type REW = Reader Env [Beam WoodProfile]

type REW2 = Reader Env (Beam WoodProfile)

data Env = Env
  { getNodeMap :: NodeMap,
    getLoadMap :: LoadMap
  }

instance FromJSON B33

instance FromJSON WoodProfileInternal

instance FromJSON YamlWoodProfile

instance FromJSON YamlPointLoad

instance FromJSON YamlLoad

instance FromJSON YamlNode

instance FromJSON YamlSystem

transformNode :: YamlNode -> Int -> Node
transformNode yn globalIndex =
  Node yn.nid yn.x yn.y (N.bitSumToDof yn.dof) globalIndex

transformNodes :: [YamlNode] -> [Node]
transformNodes yns = go yns 0 []
  where
    go :: [YamlNode] -> Int -> [Node] -> [Node]
    go [] _ acc = acc
    go (x : xs) globalIndex acc =
      let newNode = transformNode x globalIndex
          newGlobIndex = globalIndex + (N.numDof newNode.dof)
          newAcc = newNode : acc
       in go xs newGlobIndex newAcc

transformLoad :: YamlLoad -> Load
transformLoad yl =
  Load yl.lid 0.0 yl.ly1 0.0 yl.ly2 yl.f

transformLoads :: [YamlLoad] -> [Load]
transformLoads yls =
  map transformLoad yls

transformPointLoad :: NodeMap -> YamlPointLoad -> PointLoad
transformPointLoad nm yl =
  let myNode = Map.lookup yl.pnode nm
   in case myNode of
        Just hit ->
          PointLoad yl.pid yl.v yl.ang yl.pf hit
        Nothing ->
          undefined

transformPointLoads :: NodeMap -> [YamlPointLoad] -> [PointLoad]
transformPointLoads nm ploads =
  map (transformPointLoad nm) ploads

loadMap :: [Load] -> LoadMap
loadMap lds =
  let tlds = map (\x -> (x.loadId, x)) lds
   in Map.fromList tlds

createBeamProp :: NodeMap -> LoadMap -> WoodProfile -> B33 -> Maybe (BeamProp WoodProfile)
createBeamProp nm lm wp b =
  Map.lookup b.n1 nm >>= \myn1 ->
    Map.lookup b.n2 nm >>= \myn2 ->
      let myl = Map.lookup b.ld lm
       in case myl of
            Nothing ->
              Just $ BeamProp b.bid (FirstNode myn1) (SecondNode myn2) wp Nothing
            Just myl1 ->
              Just $ BeamProp b.bid (FirstNode myn1) (SecondNode myn2) wp (Just $ L.limitStates myl1)

transformB33 :: WoodProfile -> B33 -> REW2
transformB33 wp b =
  ask >>= \curEnv ->
    let nm = curEnv.getNodeMap
        lm = curEnv.getLoadMap
        prop = createBeamProp nm lm wp b
     in case prop of
          Nothing ->
            undefined
          Just p ->
            pure $ Bjlk33 p

transformWoodProfile :: String -> WoodProfileInternal -> REW
transformWoodProfile stc wpi =
  let wp = W.createWoodProfile (W.asStrengthClass stc) (Width wpi.w) (Height wpi.h)
   in mapM (transformB33 wp) wpi.b33

transformWoodProfiles' :: YamlWoodProfile -> REW
transformWoodProfiles' yp =
  mapM (transformWoodProfile yp.stclass) yp.profiles >>= \profiles ->
    pure $ concat profiles

transformWoodProfiles :: [YamlWoodProfile] -> REW
transformWoodProfiles yps =
  mapM transformWoodProfiles' yps >>= \profiles ->
    pure $ concat profiles

parseYaml' :: FilePath -> IO (Either ParseException YamlSystem)
parseYaml' fname =
  decodeFileEither fname -- "/home/rcs/opt/haskell/stearnswharf/tmp/demo1.yaml"

nodeMap :: [Node] -> NodeMap
nodeMap nodes =
  let tnodes = map (\x -> (x.nodeId, x)) nodes
   in Map.fromList tnodes

transformYamlToSystem :: YamlSystem -> System
transformYamlToSystem ymlsystem =
  let ns = transformNodes ymlsystem.nodes
      nm = nodeMap ns
      lds = transformLoads ymlsystem.loads
      ldm = loadMap lds
      env = Env nm ldm
      wp = runReader (transformWoodProfiles ymlsystem.woodprofiles) env
   in System
        { S.nodes = ns,
          S.loads = lds,
          S.pointLoads = transformPointLoads nm ymlsystem.pointloads,
          S.woodProfiles = wp
        }

parseYaml :: FilePath -> IO (Maybe System)
parseYaml fname =
  parseYaml' fname >>= \ymlsystem ->
    case ymlsystem of
      Left err ->
        (liftIO $ putStrLn $ show err)
          *> pure Nothing
      Right result ->
        pure $ Just $ transformYamlToSystem result

demoFname :: FilePath
demoFname = "/home/rcs/opt/haskell/stearnswharf/t/resources/wood1.yaml"

demo :: IO ()
demo =
  parseYaml demoFname >>= \result ->
    case result of
      Nothing ->
        putStrLn "OOPS"
      Just res1 ->
        putStrLn $ show res1

demox :: IO System
demox =
  parseYaml demoFname >>= \result ->
    pure $ fromJust result

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
