
module Besra.Repl.Parser
  ( parse
  , formatError
  , exprParser
  , declParser
  , moduleParser
  , exprOrDeclParser
  , typeParser
  , ParseError
  , Parser
  ) where

-- This module is needed since the REPL behaves slightly different
-- compared to when parsing complete files.

import Protolude hiding ( Type, try )
import Besra.Types.Ann
import Besra.Types.IR1 ( Expr, Decl, Module, Type )
import Besra.Parser ( parse, formatError, Parser, ParseError )
import Besra.Parser.Helpers ( try )
import qualified Besra.Parser.Type as Type
import qualified Besra.Parser.Expr as Expr
import qualified Besra.Parser.Module as Module


type Expr' = Expr Parsed
type Decl' = Decl Parsed
type Module' = Module Parsed
type Type' = Type Parsed

exprParser :: Parser Expr'
exprParser = Expr.parser

declParser :: Parser Decl'
declParser = Module.declParser

moduleParser :: Parser Module'
moduleParser = Module.parser

exprOrDeclParser :: Parser (Either Expr' Decl')
exprOrDeclParser = Left <$> try exprParser
                <|> Right <$> declParser

typeParser :: Parser Type'
typeParser = Type.parser

