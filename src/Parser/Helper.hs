module Parser.Helper where

import Parser.Base
import Syntax.Base
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as P

langDef :: P.LanguageDef ()
langDef =
  emptyDef
    { P.reservedNames =
        [ "val",
          "var",
          "data",
          "fun",
          "return",
          "signature",
          "instance",
          "of",
          "when",
          "loop",
          "as",
          "if",
          "else",
          "false",
          "true",
          "this",
          "imp",
          "exp"
        ],
      P.reservedOpNames =
        [ "+",
          "-",
          "*",
          "/",
          "%",
          "&&",
          "||",
          ">",
          ">=",
          "<",
          "<=",
          "=",
          "==",
          "!=",
          "->"
        ]
    }

lexer :: P.TokenParser ()
lexer = P.makeTokenParser langDef

symbol :: String -> Parser String
symbol = P.symbol lexer

reserved :: String -> Parser ()
reserved = P.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = P.reservedOp lexer

brackets :: Parser a -> Parser a
brackets = P.brackets lexer

parens :: Parser a -> Parser a
parens = P.parens lexer

braces :: Parser a -> Parser a
braces = P.braces lexer

commaSep :: Parser a -> Parser [a]
commaSep = P.commaSep lexer

commaSep1 :: Parser a -> Parser [a]
commaSep1 = P.commaSep1 lexer

colonTy :: Parser Type
colonTy = symbol ":" *> ty
