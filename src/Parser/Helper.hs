module Parser.Helper where

import Parser.Base
import Text.Parsec

parens :: Parser a -> Parser a
parens = between (char '(') (char ')')

brackets :: Parser a -> Parser a
brackets = between (char '[') (char ']')

braces :: Parser a -> Parser a
braces = between (char '{') (char '}')
