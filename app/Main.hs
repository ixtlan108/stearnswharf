{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

-- #define RCS_DEMO

main :: IO ()
main = do
  putStrLn "hi"

-- import System.Environment (getArgs)
-- import qualified Text.XML.Light as X
-- import qualified StearnsWharf.System as S
--
-- #ifdef RCS_DEMO
-- import qualified StearnsWharf.XML.XmlLoads as XL
-- import qualified StearnsWharf.XML.XmlProfiles as XP
-- #endif
--
-- main :: IO ()
-- main = do
-- #ifdef RCS_DEMO
--   --s <- readFile "/home/rcs/opt/haskell/stearnswharf/demo/project2.xml"
--   s <- readFile "/home/rcs/opt/haskell/stearnswharf/t/distload05.xml"
-- #else
--   [fileName] <- getArgs
--   s <- readFile fileName
-- #endif
--   case X.parseXMLDoc s of
--     Nothing -> error "Failed to parse xml"
--     Just doc -> S.runStearnsWharf doc
--
--
-- #ifdef RCS_DEMO
--
-- crl = XL.createLoads
--
-- cwp = XP.createWoodProfiles
--
-- demo :: IO X.Element
-- demo = do
--   s <- readFile "/home/rcs/opt/myProjects3/Odins_vei_21/xml/demo.xml"
--   case X.parseXMLDoc s of
--     Nothing -> error "Failed to parse xml"
--     Just doc -> pure doc
-- #endif
