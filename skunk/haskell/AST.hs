module AST where

import Data.ByteString.Short
import Data.List (find)
import Data.Bimap (Bimap)

data Usage = Read | Write deriving (Show, Eq)
data Type = IntType | StringType deriving (Show, Eq)
data Handle = Handle { handleName :: String, handleType :: Type, handleUsages :: [Usage]} deriving Show

isInput handle = Read `elem` handleUsages handle
isOutput handle = Write `elem` handleUsages handle

data Expression = StateReference String deriving Show

data ModuleRef = ModuleName String | InstanceName String | Position Int deriving Show

data ListenerKind = OnWrite | OnChange
data Statement = Statement { statementOutput :: String, statementExpression :: Expression}
               | ApplyTo { applyToFrom :: String, applyToModule :: ModuleRef, applyToHandle :: String } deriving Show
data Listener = Listener { listenerHandle :: String, listenerKind :: ListenerKind, listenerStatement :: Statement} deriving Show

instance Show ListenerKind where
  show OnWrite = "onWrite"
  show OnChange = "onChange"

type HandleMap = Bimap String String

data ModuleInfo = JustModule Module HandleMap | NamedModule Module String HandleMap deriving Show

data Module = Module { moduleName :: String, moduleHandles :: [Handle], moduleListeners :: [Listener], moduleSubmodules :: [ModuleInfo]} deriving Show

moduleStatements = map listenerStatement . moduleListeners

subModules mod = map justSubMod $ moduleSubmodules mod
justSubMod (JustModule m _) = m
justSubMod (NamedModule m _ _) = m

justHandleMap (JustModule _ h) = h
justHandleMap (NamedModule _ _ h) = h

inputs = filter isInput . moduleHandles
outputs = filter isOutput . moduleHandles

data Graph = Graph [String] deriving Show

data TopLevel = TLModule Module | TLGraph Graph deriving Show

-- TODO: move to a separate module
canWriteFrom :: Type -> Type -> Bool
canWriteFrom from to = from == to

handleForModule :: Module -> String -> Handle
handleForModule m hName = findWithError (\h -> handleName h == hName) (moduleHandles m) $ "No handle named " ++ hName ++ " in " ++ show m

findWithError pred input errString = case find pred input of
  Nothing -> error errString
  Just r -> r

handleForSubModuleHandle :: Module -> String -> Handle
handleForSubModuleHandle mod hName = handleForModule mod handleName
  where
    mapsTo (Statement {}) = False
    mapsTo a@(ApplyTo { }) = applyToHandle a == hName 
    applyTo = find mapsTo $ moduleStatements mod
    handleName = getHandleName applyTo
    getHandleName (Just r) = applyToFrom r
    getHandleName Nothing = hName
