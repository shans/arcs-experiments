{-# LANGUAGE NamedFieldPuns #-}
module Parser where

import Text.Parsec as P hiding (Empty)
import Data.Set as S hiding (foldl, filter)
import Data.List as L
import Data.Char
import Data.Maybe

import AST

parse :: String -> String -> Either ParseError [TopLevel]
parse = P.parse parseToplevels


mkModule :: String -> [Handle] -> [Listener] -> Module
mkModule moduleName moduleHandles moduleListeners = Module { moduleName, moduleHandles, moduleListeners, moduleSubmodules=[] }

mkHandle :: String -> [Usage] -> Type -> Handle
mkHandle handleName handleUsages handleType = Handle { handleName, handleUsages, handleType }

mkListener :: String -> ListenerKind -> Statement -> Listener
mkListener listenerHandle listenerKind listenerStatement = Listener { listenerHandle, listenerKind, listenerStatement }

mkStatement :: String -> Expression -> Statement
mkStatement statementOutput statementExpression = Statement { statementOutput, statementExpression }

parseToplevels :: Parsec String () [TopLevel]
parseToplevels = many1 (try $ parseModuleAsTL <|> parseGraphAsTL) <* spaces <* eof

parseModuleAsTL :: Parsec String () TopLevel
parseModuleAsTL = TLModule <$> try parseModule

parseModule :: Parsec String () Module
parseModule = mkModule <$> (moduleStr *> name) <*> (openBody *> handles) <*> (listeners <* closeBody)
  where
    moduleStr  = spaces *> P.string "module"
    openBody   = spaces *> P.char '{'
    closeBody  = spaces *> P.char '}'
    listeners  = many (try $ spaces *> listener)

spaceChars :: String
spaceChars = " \n"

name :: Parsec String () String
name = spaces *> many1 (noneOf $ specialChars)

specialChars :: String
specialChars = ":;!{}[]()." ++ spaceChars

handles :: Parsec String () [Handle]
handles = many (try $ spaces *> handle)

handle :: Parsec String () Handle
handle = mkHandle <$> name <*> (colon *> usages) <*> hType <* spaces <* semicolon

hType :: Parsec String () Type
hType = spaces *> ((when "Int" IntType) <|> (when "String" StringType))

semicolon :: Parsec String () Char
semicolon = spaces *> P.char ';'

colon :: Parsec String () Char
colon = spaces *> P.char ':'

dot :: Parsec String () Char
dot = P.char '.'

equals :: Parsec String () Char
equals = spaces *> P.char '='

usages :: Parsec String () [Usage]
usages = many1 (try usage)

usage :: Parsec String () Usage
usage = spaces *> ((when "reads" Read) <|> (when "writes" Write))

-- utility for parsing string token alternatives 
when :: String -> a -> Parsec String () a
when s v = try $ P.string s *> return v

listeners :: Parsec String () [Listener]
listeners = many (try $ spaces *> listener)

listener :: Parsec String () Listener
listener = mkListener <$> name <*> (dot *> kind) <*> (colon *> statement) <* semicolon

kind :: Parsec String () ListenerKind
kind = ((when "onChange" OnChange) <|> (when "onWrite" OnWrite))

statement :: Parsec String () Statement
-- TODO: Move spaces before expression into expression capture once it's more than just a reference.
statement = mkStatement <$> (spaces *> name) <*> (spaces *> P.string "<-" *> spaces *> expression)

expression :: Parsec String () Expression
expression = StateReference <$> name

mkGraph h t = Graph $ h:t

parseGraphAsTL :: Parsec String () TopLevel
parseGraphAsTL = TLGraph <$> parseGraph

parseGraph :: Parsec String () Graph
parseGraph = mkGraph <$> name <*> many1 (try $ arrow *> name) <* semicolon
  where
      arrow = spaces *> P.string "->"

modules :: [TopLevel] -> [Module]
modules = mapMaybe asModule

graphs :: [TopLevel] -> [Graph]
graphs = mapMaybe asGraph

asModule :: TopLevel -> Maybe Module
asModule (TLModule m) = Just m
asModule _ = Nothing

asGraph :: TopLevel -> Maybe Graph
asGraph (TLGraph g) = Just g
asGraph _ = Nothing