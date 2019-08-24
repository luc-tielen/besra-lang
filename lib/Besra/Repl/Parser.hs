
module Besra.Repl.Parser
  ( parse
  , formatError
  , exprParser
  , declParser
  , exprOrDeclParser
  , ParseError
  ) where

-- This module is needed since the REPL behaves slightly different
-- compared to when parsing complete files.

import Protolude hiding ( try )
import Besra.Types.Ann
import Besra.Types.IR1 ( Expr, Decl )
import Besra.Parser ( parse, formatError, Parser, ParseError )
import Besra.Parser.Helpers ( try )
import qualified Besra.Parser.Expr as Expr
import qualified Besra.Parser.Module as Module


type Expr' = Expr 'Parsed
type Decl' = Decl 'Parsed

exprParser :: Parser Expr'
exprParser = Expr.parser

declParser :: Parser Decl'
declParser = Module.declParser

exprOrDeclParser :: Parser (Either Expr' Decl')
exprOrDeclParser = Left <$> try exprParser
                <|> Right <$> declParser

