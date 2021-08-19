{-# LANGUAGE TemplateHaskell #-}
module TestParser where

import Test.Tasty
import Test.QuickCheck
import Test.QuickCheck.All
import Test.QuickCheck.Modifiers
import Test.QuickCheck.Gen
import Test.Tasty.HUnit
import Parser
import Text.Parsec as P
import Data.List

nameChar :: Gen Char
nameChar = elements "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

nameString :: Gen String
nameString = listOf1 nameChar

newtype NameString = NameString {getNameString :: String} deriving (Eq, Ord, Show, Read)

instance Arbitrary NameString where
  arbitrary = fmap NameString nameString

handleDirections :: Gen [String]
handleDirections = fmap nub . listOf1 $ elements ["reads", "writes"]

newtype HandleDirections = HandleDirections {getHandleDirections :: [String]} deriving (Eq, Ord, Show, Read)

instance Arbitrary HandleDirections where
  arbitrary = fmap HandleDirections handleDirections

typeString :: Gen String
typeString = elements ["Int", "String"]

newtype TypeString = TypeString {getType :: String} deriving (Eq, Ord, Show, Read)

instance Arbitrary TypeString where
  arbitrary = fmap TypeString typeString

join = foldl1 (\a b -> a ++ " " ++ b)

handleString :: NameString -> HandleDirections -> TypeString -> Gen String
handleString n d t = 
  let name = getNameString n
      directions = getHandleDirections d
      theType = getType t in
  return $ name ++ ": " ++ join directions ++ " " ++ theType ++ ";"

newtype HandleString = HandleString {getHandle :: String} deriving (Eq, Ord, Show, Read)

instance Arbitrary HandleString where
  arbitrary = do
    name <- arbitrary
    directions <- arbitrary
    theType <- arbitrary
    handle <- handleString name directions theType
    return $ HandleString handle


willParse :: String -> Parsec String () a -> Bool
willParse s parser = case P.parse parser "" s of 
  Left _  -> False
  Right _ -> True

----------------------

prop_parsesHandle :: HandleString -> Property
prop_parsesHandle theHandle = collect (theHandle) $ willParse (getHandle theHandle) handle

prop_parsesEmptyModule :: NameString -> Property
prop_parsesEmptyModule name' = collect (length name) (willParse ("module " ++ name ++ " {}") parseModule)
  where
    name = getNameString name'

prop_parsesModuleWithInput :: NameString -> NameString -> Bool
prop_parsesModuleWithInput n i = 
  let name = getNameString n
      input = getNameString i in
  willParse ("module " ++ name ++ " {\n  " ++ input ++ ": reads Int;\n}\n") parseModule

prop_parsesModuleWithListener :: NameString -> NameString -> NameString -> Bool
prop_parsesModuleWithListener n i o =
  let name   = getNameString n
      input  = getNameString i
      output = getNameString o in
  willParse ("module " ++ name ++ " {\n" ++ input ++ ": reads Int;\n" ++ output ++ ": writes Int;\n" ++ input ++ ".onChange: " ++ output ++ " <- " ++ input ++ ";\n}\n") parseModule

---------------------

timesTwo = "module TimesTwo {\n  in: reads Int;\n  out: writes Int;\n  in.onChange: out <- in;\n}\n"
displayChange = "module DisplayChange {\n  in: reads Int;\n  out: writes String;\n  in.onWrite: out <- in;\n}\n"

simpleGraph = "TimesTwo -> DisplayChange;\n"
otherGraph = "TimesTwo -> DisplayChange -> TimesTwo -> DisplayChange;\n"

multi = foldl1 (\a b -> a ++ "\n" ++ b)

parserTests = testGroup "parser unit tests"
  [ testCase "Can parse a simple module" $
      willParse timesTwo parseModule @?= True
  , testCase "Can parse another simple module" $
      willParse displayChange parseModule @?= True
  , testCase "Can parse a simple graph" $
      willParse  simpleGraph parseGraph @?= True
  , testCase "Can parse a listener" $
      willParse "input.onChange: output <- input;\n" listener @?= True
  , testCase "Can parse a statement" $
      willParse "output <- input" statement @?= True
  , testCase "Can parse multiple modules" $
      willParse (multi [timesTwo, displayChange]) parseToplevels @?= True
  , testCase "Can parse multiple toplevels" $
      willParse (multi [timesTwo, displayChange, simpleGraph]) parseToplevels @?= True

  ]


---------------------
return []
runTests :: IO Bool
runTests = $quickCheckAll
