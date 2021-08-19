{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module GraphToModule where

import Graph hiding (Handle)
import AST hiding (Graph)
import Data.List ((\\), find)

import qualified Data.Bimap as B

{-

A module can wrap a collection of other modules that are organized into a connection graph. When this happens:
* all handles and all unattached connections end up in the containing module's state
* unattached read connections have an OnWrite listener installed; so do handles that are connected to read connections.

e.g.

  c1 -> X -> c2 -> h1 -> c3 -> Y -> c4

(cN - connection, X, Y - modules, h1 - handle)

c1, h, and c4 will form the containing module's state (each will be a "moduleHandle")
c1 and h will have a listener installed

When a listener is triggered, it must:
 - copy the change from the containing module's state to the appropriate contained modules state
 - apply the containing module state (move the value from the update to the state)
 - trigger processing of the contained module
 - copy any updated state from relevant channels of the contained module back into the containing module state
 - if the output handle is write-only for the contained module; apply the update to the contained module.

This functionality is encapsulated as an applyTo statement.

Note that this is going to get more complicated once multiple readers/writers are supported :)

What needs to be known? Each input maps to a set of (module, handle) pairs. The above example would look something like:

module Outer {
  c1: reads T;
  h1: reads writes T';
  c4: writes T'';

  c1.onWrite: applyTo [(X, c1)]
  h1.onWrite: applyTo [(Y, c3)]

  // children: X, Y
}

-}

type GTMContext = (Graph, [Module])

class GTMModuleIndex a where
  toModule :: GTMContext -> a -> Module

instance GTMModuleIndex Module where
  toModule _ a = a

instance GTMModuleIndex String where
  toModule (_, ms) a = findModule a ms
  
instance GTMModuleIndex Int where
  toModule c a = modIndexToModule c a

class GTMHandleIndex a where
  toHandle :: GTMContext -> a -> Handle
  
instance GTMHandleIndex (Int, String) where
  toHandle c (m, name) = toHandle c (toModule c m, name)

instance GTMHandleIndex (Module, String) where
  toHandle _ (m, name) = handleForModule m name

instance GTMHandleIndex Handle where
  toHandle _ a = a

graphToModule :: String -> GTMContext -> Module
graphToModule name context@(graph, mods) = Module { moduleName = name, moduleHandles = connections, moduleListeners = listeners, moduleSubmodules = submodules}
  where
    connections = forwardedConnections ++ subHandles
    forwardedConnections = map (toHandle context) cData
    subHandles = map handleToModuleHandle $ handles graph
    -- each connection in cData is (Int, String) - module IDX, handle name
    cData = freeConnections context
    readCData = filter (\a -> isInput $ toHandle context a ) cData
    listeners = connectionListeners ++ subHandleListeners
    connectionStatements = map (\(m, name) -> ApplyTo name (Position m) name) readCData
    connectionListeners = map (\s@(ApplyTo _ _ name) -> Listener { listenerHandle = name, listenerKind = OnWrite, listenerStatement = s }) connectionStatements
    subHandleListeners = map (listenerDataToListener . handleIDXToListenerData context) [0..length (handles graph) - 1]
    submodules = map moduleInfo $ [0.. length (modules graph) - 1]
    moduleInfo idx = JustModule mod mapping
      where
        mod = toModule context idx
        mapping = moduleToSubmoduleMapping context idx

handleToModuleHandle (name, hType) = Handle { handleName = name, handleType = hType, handleUsages = [Read, Write] }

moduleToSubmoduleMapping :: GTMContext -> Int -> B.Bimap String String
moduleToSubmoduleMapping context@(graph, mods) mIdx = B.fromList $ fcMap ++ (handlesConnectedToModule graph mIdx)
  where
    freeConnections = freeConnectionsForModule context mIdx
    fcMap = map (\a -> (a, a)) freeConnections
    
-- this only deals with a single write handle, will need to change that.
handleIDXToListenerData (graph, mods) idx = (triggerName, applyToName, moduleRef)
  where
    triggerName = fst graphHandle
    graphHandle = handles graph !! idx
    applyToName = connection
    [(_, (_, cidx))] = arrowsByPred graph (outputConnectionForHandle idx)
    connection = connections graph !! cidx
    [(_, (_, midx))] = arrowsByPred graph (inputConnectionToModule cidx)
    moduleRef = Position midx

listenerDataToListener (triggerName, applyToName, moduleRef) = Listener { 
  listenerHandle = triggerName, 
  listenerKind = OnWrite, 
  listenerStatement = ApplyTo triggerName moduleRef applyToName 
}

freeConnections :: GTMContext -> [(Int, String)]
freeConnections context@(graph, mods) = concat $ map freeConnectionsForModuleWithModule [0..length (modules graph) - 1]
  where
    freeConnectionsForModuleWithModule m = map (\a -> (m, a)) $ freeConnectionsForModule context m

freeConnectionsForModule :: GTMContext -> Int -> [String]
freeConnectionsForModule context@(graph, mods) m = allHandles \\ connectedHandles
  where
    -- handles from the graph
    connectedHandles = connectionsForModule graph m
    -- handles from the module definition
    allHandles = map handleName $ moduleHandles mod
    mod = toModule context m

-----

findModule :: String -> [Module] -> Module
findModule name modList =
  case find (\mod -> AST.moduleName mod == name) modList of
    Nothing -> error $ "no module matching name " ++ name ++ " found in list of modules"
    Just r  -> r
  
modIndexToModule :: GTMContext -> Int -> Module
modIndexToModule (graph, mods) m = mod
  where
    modName = (modules graph) !! m
    mod = findModule modName mods

