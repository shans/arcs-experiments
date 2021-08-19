{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module IRHelpers where

import LLVM.IRBuilder.Instruction as I
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import LLVM.AST hiding (global, function)
import LLVM.AST.Type as Type
import LLVM.AST.Constant as Constant
import LLVM.AST.AddrSpace

import Data.Text.Encoding (encodeUtf8)
import Data.Text (pack)
import Data.ByteString.Short (toShort)

fromString = toShort . encodeUtf8 . pack

i64_0 = Int { integerBits=64, integerValue=0 }

iop32 :: Int -> Operand
iop32 i = ConstantOperand $ icon32 i

iop64 :: Int -> Operand
iop64 i = ConstantOperand $ icon64 i

icon32 :: Int -> Constant
icon32 i = Int { integerBits=32, integerValue=(toInteger i) }

icon64 :: Int -> Constant
icon64 i = Int { integerBits=64, integerValue=(toInteger i) }

allocateAndStore vType value name = do
  stackArg <- alloca vType Nothing 0 `named` name
  store stackArg 0 value
  return stackArg
