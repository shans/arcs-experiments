import System.Exit
import System.IO

import LLVM.Pretty
import LLVM.Target
import LLVM.Module
import LLVM.Context
import LLVM.AST

import qualified Data.Text.Lazy.IO as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Short as BS 
import qualified Data.ByteString.Char8 as C8
import Data.String (IsString)

import AST
import Parser (parse, graphs, modules)
import IRGen
import GraphBuilder (mkGraph, resolveGraph)
import GraphToModule (graphToModule)

parseOrDie :: String -> String -> IO [TopLevel]
parseOrDie src str = case parse src str of
  Left error -> die $ show error
  Right result -> return result

main :: IO ()
main = do
  source <- readFile "../cpp/test.skunk"
  ast <- parseOrDie "../cpp/test.skunk" source
  putStrLn $ show ast
  let graph = mkGraph $ graphs ast
  putStrLn $ show graph
  let resolvedGraph = resolveGraph graph (modules ast)
  putStrLn $ show resolvedGraph
  let gMod = graphToModule "Main" (resolvedGraph, (modules ast))
  putStrLn $ show gMod
  let irs = genIR gMod
  sequence_ $ map irToObject irs

irToObject ir = do
  T.putStrLn $ ppllvm ir
  withContext $ \context -> do
    withModuleFromAST context ir $ \mod -> do
      withHostTargetMachineDefault $ \targetMachine -> do
        writeObjectToFile targetMachine (File $ LLVM.AST.moduleName ir `plus` ".o") mod
  
plus :: BS.ShortByteString -> String -> FilePath
plus sbs bs = (C8.unpack $ BS.fromShort sbs) ++ bs