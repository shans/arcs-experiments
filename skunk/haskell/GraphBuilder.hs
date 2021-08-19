module GraphBuilder where

import Control.Monad.State
import Data.List (find)

import Graph
import qualified AST as AST

mkGraph gs = execState (mkGraph_m gs) (Graph [] [] [] [])
resolveGraph g ms = execState (resolveGraph_m ms) g

mkGraph_m :: [AST.Graph] -> State Graph ()
mkGraph_m graphs = foldM (\_ g -> addGraph g) () graphs
  
  
addGraph :: AST.Graph -> State Graph ()  
addGraph (AST.Graph g) = do
  idxs <- sequence $ map addModuleM g
  let zippedIdxs = zip idxs (tail idxs)
  sequence $ map (\(a, b) -> connectModuleToModuleM a b) zippedIdxs
  return ()  
  

resolveGraph_m :: [AST.Module] -> State Graph ()
resolveGraph_m modules = do
  connections <- filterModuleToModuleConnectionsM
  sequence $ map (\c -> m2mTom2c2h modules c) connections
  return ()

m2mTom2c2h :: [AST.Module] -> Arrow -> State Graph ()
m2mTom2c2h modules ((ModuleEndpoint, a), (ModuleEndpoint, b)) = do
  modA <- findModule a modules
  modB <- findModule b modules
  let (output, input, hType) = onlyMatchingConnection modA modB
  cA <- addConnectionM output
  cB <- addConnectionM input
  connectModuleToConnectionM a cA
  connectConnectionToModuleM cB b
  let hName = AST.moduleName modA ++ "-" ++ output ++ "-" ++ input ++ "-" ++ AST.moduleName modB
  handle <- addHandleM hName hType
  connectConnectionToHandleM cA handle
  connectHandleToConnectionM handle cB
  return ()

findModule :: Int -> [AST.Module] -> State Graph AST.Module
findModule modIdx modList = do
  graph <- get
  let modName = (modules graph) !! modIdx
  return $ case find (\mod -> AST.moduleName mod == modName) modList of
    Nothing -> error $ "no module matching name " ++ modName ++ " found in list of modules"
    Just r  -> r

onlyMatchingConnection :: AST.Module -> AST.Module -> (String, String, AST.Type)
onlyMatchingConnection a b = case matchingConnections a b of
  [r] -> r
  []  -> error $ "no matching connection from " ++ show a ++ " to " ++ show b
  _   -> error $ "multiple matching connections from " ++ show a ++ " to " ++ show b

-- TODO: typesMatch should return a handle type (and probably be renamed), that should be returned as the third component rather than just the LHS type.
matchingConnections a b = [(AST.handleName l, AST.handleName r, AST.handleType l) | l <- AST.outputs a, r <- AST.inputs b, typesMatch l r]

typesMatch h1 h2 = AST.canWriteFrom (AST.handleType h1) (AST.handleType h2)