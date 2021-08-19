module Graph where

import Control.Monad.State
import Data.List (partition)
import AST (Type)

data EndpointType = ModuleEndpoint | ConnectionEndpoint | HandleEndpoint deriving (Eq, Show)
type Endpoint = (EndpointType, Int)
type Arrow = (Endpoint, Endpoint)
type EdgePred = Arrow -> Bool
type Handle = (String, Type)
data Graph = Graph { modules :: [String], connections :: [String], handles :: [Handle], arrows :: [(Endpoint, Endpoint)]} deriving Show

addModule :: Graph -> String -> (Graph, Int)
addModule g m = (g { modules = modules g ++ [m] }, length $ modules g)

addModuleM = m1 addModule

addConnection :: Graph -> String -> (Graph, Int)
addConnection g c = (g { connections = connections g ++ [c] }, length $ connections g )

addConnectionM = m1 addConnection

addHandle :: Graph -> String -> Type -> (Graph, Int)
addHandle g h t = (g { handles = handles g ++ [(h, t)] }, length $ handles g )

addHandleM = m2 addHandle

connectModuleToModule = connect m2m
connectModuleToModuleM = m2 connectModuleToModule

connectModuleToConnection = connect m2c
connectModuleToConnectionM = m2 connectModuleToConnection

connectConnectionToModule = connect c2m
connectConnectionToModuleM = m2 connectConnectionToModule

connectConnectionToHandle = connect c2h
connectConnectionToHandleM = m2 connectConnectionToHandle

connectHandleToConnection = connect h2c
connectHandleToConnectionM = m2 connectHandleToConnection

moduleToModuleConnections :: Graph -> [Arrow]
moduleToModuleConnections g = filter (edgePred ModuleEndpoint ModuleEndpoint) $ arrows g

filterModuleToModuleConnections :: Graph -> (Graph, [Arrow])
filterModuleToModuleConnections g = filterArrowsByPred g $ edgePred ModuleEndpoint ModuleEndpoint

filterModuleToModuleConnectionsM = m0 filterModuleToModuleConnections

---

connectionsForModule :: Graph -> Int -> [String]
connectionsForModule g m = map ((connections g) !!) connectionRefs
  where
    connectionRefs = map (extractRef ConnectionEndpoint) arrows
    arrows = arrowsByPred g $ isConnectionForModule m

-- returns [(handle, connection)] for all handles connected to the given module ID
handlesConnectedToModule :: Graph -> Int -> [(String, String)]
handlesConnectedToModule g m = concat $ map handles connectionRefs
  where
    connectionRefs = map (extractRef ConnectionEndpoint) arrows
    arrows = arrowsByPred g $ isConnectionForModule m
    handles idx = map (\a -> (a, name)) handleNames
      where
        handleNames = handlesConnectedToConnection g idx
        name = (connections g) !! idx

handlesConnectedToConnection :: Graph -> Int -> [String]
handlesConnectedToConnection g c = map (fst . ((handles g) !!)) handleRefs 
  where
    handleRefs = map (extractRef HandleEndpoint) arrows
    arrows = arrowsByPred g $ isHandleForConnection c


---

connect :: (Int -> Int -> Arrow) -> Graph -> Int -> Int -> (Graph, Int)
connect f g a b = (g { arrows = arrows g ++ [f a b] }, length $ arrows g)

arrowsByPred :: Graph -> EdgePred -> [Arrow]
arrowsByPred g pred = filter pred $ arrows g

filterArrowsByPred :: Graph -> EdgePred -> (Graph, [Arrow])
filterArrowsByPred g pred = (g { arrows = rest }, matching)
  where
    (matching, rest) = partition pred $ arrows g

edgePred l r ((e1, _), (e2, _)) = l == e1 && r == e2

bidiPred a b  arr = edgePred a b arr || edgePred b a arr

lhsMatches t v ((lt, lv), _) = lt == t && lv == v
rhsMatches t v (_, (rt, rv)) = rt == t && rv == v
lhsType t ((lt, _), _) = lt == t
rhsType t (_, (rt, _)) = rt == t

bidiMatchTypePred t v ot a = lhsMatches t v a && rhsType ot a || lhsType ot a && rhsMatches t v a

isConnectionForModule m = bidiMatchTypePred ModuleEndpoint m ConnectionEndpoint

isHandleForConnection c = bidiMatchTypePred ConnectionEndpoint c HandleEndpoint

outputConnectionForHandle h a = lhsMatches HandleEndpoint h a && rhsType ConnectionEndpoint a

inputConnectionToModule c a = lhsMatches ConnectionEndpoint c a && rhsType ModuleEndpoint a

extractRef t a@((u, _), (v, _)) | t == u && t == v    = error $ "multiple " ++ show t ++ " references in " ++ show a
extractRef t ((u, v), _)        | t == u              = v
extractRef t (_, (u, v))        | t == u              = v
extractRef t a                                        = error $ "no " ++ show t ++ " references in " ++ show a

m0 :: (a -> (a, b)) -> State a b
m0 f = do
  a <- get
  let (a', b) = f a
  put a'
  return b

m1 :: (a -> b -> (a, c)) -> b -> State a c
m1 f b = do
  a <- get
  let (a', c) = f a b
  put a'
  return c

m2 :: (a -> b -> c -> (a, d)) -> b -> c -> State a d
m2 f b c = do
  a <- get
  let (a', d) = f a b c
  put a'
  return d

m2m a b = ((ModuleEndpoint, a), (ModuleEndpoint, b))
m2c a b = ((ModuleEndpoint, a), (ConnectionEndpoint, b))
c2m a b = ((ConnectionEndpoint, a), (ModuleEndpoint, b))
c2h a b = ((ConnectionEndpoint, a), (HandleEndpoint, b))
h2c a b = ((HandleEndpoint, a), (ConnectionEndpoint, b))