{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module IRGen where

import LLVM.IRBuilder.Instruction as I
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import LLVM.AST hiding (global, function)
import LLVM.AST.Type as Type
import LLVM.AST.Constant as Constant
import LLVM.AST.IntegerPredicate
import LLVM.AST.AddrSpace

import Data.Bimap ((!>))
import Data.List (findIndex)

import Data.Bits (shift, complement)

import qualified AST
import IRHelpers

dup :: [a] -> [a]
dup [] = []
dup (h:t) = h:h:dup t

class IrTypeable t where
  irType :: t -> Type
  irConstant :: t -> Constant

instance IrTypeable AST.Handle where
  irType _ = i64
  irConstant _ = i64_0

instance IrTypeable AST.Module where  
  irType theModule = StructureType { Type.isPacked=False, elementTypes=handleMembers}
    where 
      handleMembers = (dup $ map irType handles) ++ [i64] ++ (map irType submodules)
      handles = AST.moduleHandles theModule
      submodules = AST.subModules theModule
  irConstant theModule = Struct { structName=Nothing, Constant.isPacked=False, memberValues=handleConstants }
    where
      handleConstants = (dup $ map irConstant handles) ++ [i64_0] ++ (map irConstant submodules)
      handles = AST.moduleHandles theModule
      submodules = AST.subModules theModule

moduleBitfieldOffset :: AST.Module -> Int
moduleBitfieldOffset mod = (length mHandles) * 2
  where
    mHandles = AST.moduleHandles mod

moduleStatePosForName :: AST.Module -> String -> Int
moduleStatePosForName mod name = case findIndex (\h -> AST.handleName h == name) (AST.moduleHandles mod) of
  Nothing -> error $ "Attempt to lookup state member " ++ name ++ " failed for " ++ (AST.moduleName mod)
  Just pos -> pos

listenersForHandle :: AST.Module -> AST.Handle -> [(AST.Listener, Int)]
listenersForHandle mod handle = filter (\(l, _) -> AST.listenerHandle l == AST.handleName handle) $ indexedListeners 
  where
    indexedListeners = zip (AST.moduleListeners mod) [0..] 

genIR :: AST.Module -> [Module]
genIR m = thisM:subMs
  where
  thisM = genModuleIR m
  subMs = concat . map genIR $ AST.subModules m

genModuleIR :: AST.Module -> Module
genModuleIR m = buildModule (fromString mName) $ do
  global (Name $ fromString ("__s_" ++ mName)) (irType m) (irConstant m)
  listenerImpls <- sequence $ map (\l -> genListenerIR l m) mListeners
  genUpdateFunctionIR m listenerImpls
  where
    mName = AST.moduleName m
    mListeners = AST.moduleListeners m

genUpdateFunctionIR mod listenerImpls = do
  function (Name functionName) argTypes VoidType $ \[state] -> do
    entry <- block `named` "entry"
    stackArg <- allocateAndStore (ptr mType) state "stateAlloca"
    statePtr <- load stackArg 0 `named` "statePtr"
    bitfieldPtr <- getBitfieldPtr mod statePtr
    bitfield <- load bitfieldPtr 0 `named` "bitfield"
    forEachHandle $ \handle -> do
      let listeners = listenersForHandle mod handle
      let hName = AST.handleName handle
      let position = moduleStatePosForName mod hName
      bitfieldTest <- I.and bitfield (iop64 $ 1 `shift` position) `named` "bitfieldTest"
      hasUpdate <- icmp NE bitfieldTest (iop64 0) `named` "hasUpdate"
      let activateFor = Name $ fromString ("activateFor_" ++ AST.handleName handle)
      let after = Name $ fromString ("after_" ++ AST.handleName handle)
      condBr hasUpdate activateFor after

      emitBlockStart activateFor
      writePtr <- getValuePtr mod statePtr hName
      updatePtr <- getUpdatePtr mod statePtr hName
      clearBitfield mod bitfieldPtr hName
      update <- load updatePtr 0 `named` "value"
      store writePtr 0 update
      store updatePtr 0 (iop64 0)

      forEachListener listeners $ \(_, idx) -> do
        call (listenerImpls !! idx) [(statePtr, [])]

      br after

      emitBlockStart after
    retVoid
  where
    functionName = fromString $ mName ++ "_update"
    mName = AST.moduleName mod
    argTypes = [(ptr mType, "state")]
    mType = irType mod
    mHandles = AST.moduleHandles mod
    forEachHandle f = do
      sequence $ map f mHandles
      return ()
    forEachListener listeners f = do
      sequence $ map f listeners
      return ()
    
genListenerIR :: MonadModuleBuilder m => AST.Listener -> AST.Module -> m Operand
genListenerIR listener mod = do
  function (Name functionName) argTypes VoidType $ \[state] -> genStatementIR lStatement mod state
    where
      functionName = fromString $ mName ++ "__" ++ (show lKind) ++ "__" ++ lHandle
      argTypes = [(ptr mType, "state")]
      lStatement = AST.listenerStatement listener
      lKind = AST.listenerKind listener
      lHandle = AST.listenerHandle listener
      mName = AST.moduleName mod
      mType = irType mod

genStatementIR :: (MonadIRBuilder m, MonadModuleBuilder m) => AST.Statement -> AST.Module -> Operand -> m ()
genStatementIR statement@AST.Statement { } mod state = do
  entry <- block `named` "entry"; do
    stackArg <- allocateAndStore (ptr mType) state "stateAlloca"
    returnVal <- genExpressionIR sExpression mod stackArg
    statePtr <- load stackArg 0 `named` "statePtr"
    bitfieldPtr <- getBitfieldPtr mod statePtr
    setBitfield mod bitfieldPtr sOutput
    updatePtr <- getUpdatePtr mod statePtr sOutput
    store updatePtr 0 returnVal
    retVoid
      where
        sExpression = AST.statementExpression statement
        sOutput = AST.statementOutput statement
        mType = irType mod
        mBitfieldOffset = moduleBitfieldOffset mod
        mOutputPos = moduleStatePosForName mod sOutput
genStatementIR applyTo@AST.ApplyTo { } mod state = do
  entry <- block `named` "entry"; do
    stackArg <- allocateAndStore (ptr mType) state "stateAlloca"
    genApplyToIR applyTo mod stackArg    
      where
        mType = irType mod

genExpressionIR :: (MonadIRBuilder m, MonadModuleBuilder m) => AST.Expression -> AST.Module -> Operand -> m Operand
genExpressionIR (AST.StateReference stateVar) mod stackArg = do
  statePtr <- load stackArg 0 `named` "statePtr"
  valuePtr <- getValuePtr mod statePtr stateVar
  load valuePtr 0 `named` "value"

genApplyToIR :: (MonadIRBuilder m, MonadModuleBuilder m) => AST.Statement -> AST.Module -> Operand -> m ()
genApplyToIR applyTo mod stackArg = do
  let modRef = AST.applyToModule applyTo
  let modPos = findModulePositionFromRef modRef mod
  let copyFromName = AST.applyToFrom applyTo
  statePtr <- load stackArg 0 `named` "statePtr"
  fromValuePtr <- getValuePtr mod statePtr copyFromName
  submoduleState <- getSubmodulePtr mod statePtr modPos
  let submoduleInfo = AST.moduleSubmodules mod !! modPos
  let submodule = AST.justSubMod submoduleInfo 
  let copyToName = AST.applyToHandle applyTo
  toUpdatePtr <- getUpdatePtr submodule submoduleState copyToName
  value <- load fromValuePtr 0
  store toUpdatePtr 0 value
  bitmapPtr <- getBitfieldPtr submodule submoduleState
  setBitfield submodule bitmapPtr copyToName
  loopStart <- fresh `named` "loopStart"
  br loopStart

  emitBlockStart loopStart
  invokeSubmodule submodule submoduleState
  bitfieldPtr <- getBitfieldPtr submodule submoduleState
  bitfield <- load bitfieldPtr 0 `named` "bitfield"

  let handleIdxs = [0..length (AST.moduleHandles submodule) - 1]
  sequence $ map (copyBack mod statePtr submoduleInfo submoduleState bitfield) handleIdxs

  hasUpdate <- icmp NE bitfield (iop64 0) `named` "hasUpdate"
  let updatesComplete = Name $ fromString "updatesComplete"
  condBr hasUpdate loopStart updatesComplete

  emitBlockStart updatesComplete
  return ()

copyBack :: (MonadIRBuilder m, MonadModuleBuilder m) => AST.Module -> Operand -> AST.ModuleInfo -> Operand -> Operand -> Int -> m ()
copyBack mod modState submodInfo submodState bitfield i = do
  mask <- shl (iop64 1) (iop64 i)
  test <- I.and bitfield mask `named` "test"
  needsCopy <- icmp NE test (iop64 0) `named` "needsCopy" 
  copyName <- fresh `named` "copy"
  nextName <- fresh `named` "next"
  condBr needsCopy copyName nextName
  emitBlockStart copyName
  let submod = AST.justSubMod submodInfo
  let submodHandleMap = AST.justHandleMap submodInfo
  let srcHandle = (AST.moduleHandles submod) !! i
  let srcName = AST.handleName srcHandle
  srcPtr <- getUpdatePtr submod submodState srcName
  let destName = submodHandleMap !> srcName
  destPtr <- getUpdatePtr mod modState destName
  value <- load srcPtr 0
  store destPtr 0 value
  bitfieldPtr <- getBitfieldPtr mod modState
  setBitfield mod bitfieldPtr destName
  br nextName
  emitBlockStart nextName
  return ()



invokeSubmodule mod statePtr = do
  let mType = irType mod
  let argTypes = [ptr mType]
  let mName = AST.moduleName mod
  let functionName = mName ++ "_update"
  functionRef <- extern (mkName functionName) argTypes VoidType
  call functionRef [(statePtr, [])]
  return ()
  
findModulePositionFromRef (AST.Position m) mod = m

getSubmodulePtr mod statePtr modPos = do
  let firstSubmodStatePos = length (AST.moduleHandles mod) * 2 + 1
  gep statePtr [iop32 0, iop32 $ firstSubmodStatePos + modPos] `named` "submodState"

getValuePtr mod statePtr output = do
  gep statePtr [iop32 0, iop32 $ mOutputPos * 2] `named` "valuePtr"
  where
    mOutputPos = moduleStatePosForName mod output

getUpdatePtr mod statePtr output = do
  gep statePtr [iop32 0, iop32 $ mOutputPos * 2 + 1] `named` "updatePtr"
  where
    mOutputPos = moduleStatePosForName mod output

getBitfieldPtr mod statePtr = do
  gep statePtr [iop32 0, iop32 mBitfieldOffset] `named` "bitfieldPtr"
  where
    mBitfieldOffset = moduleBitfieldOffset mod

setBitfield mod bitfieldPtr output = do
  bitfield <- load bitfieldPtr 0 `named` "bitfield"
  newBitfield <- I.or bitfield $ iop64 (1 `shift` position)
  store bitfieldPtr 0 newBitfield
  where
    position = moduleStatePosForName mod output

clearBitfield mod bitfieldPtr output = do
  bitfield <- load bitfieldPtr 0 `named` "bitfield"
  newBitfield <- I.and bitfield $ iop64 (complement $ 1 `shift` position)
  store bitfieldPtr 0 newBitfield
  where
    position = moduleStatePosForName mod output

