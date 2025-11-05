module Parser.ExpressionSpec where

import Parser.Base
import Parser.Expression
import Syntax.Base
import Syntax.Expression
import Syntax.Literal
import Test.Hspec
import Text.Parsec

spec :: Spec
spec = do
  describe "expression list" $ do
    it "should parse a raw expression list" $ do
      parse expressionList "" "true,32,!true,2+3"
        `shouldBe` Right
          ( RawExpressionList
              [ LiteralExpression $ BooleanLiteral True,
                LiteralExpression $ IntegerLiteral Dec "32",
                UnaryExpression Not (LiteralExpression $ BooleanLiteral True),
                BinaryExpression
                  Plus
                  (LiteralExpression $ IntegerLiteral Dec "2")
                  (LiteralExpression $ IntegerLiteral Dec "3")
              ]
          )

    it "should parse a labeled expression list" $ do
      parse expressionList "" "bool:true,int:32,unary:!true,binary:2+3"
        `shouldBe` Right
          ( LabeledExpressionList
              [ LabeledExpression
                  (Identifier "bool")
                  (LiteralExpression $ BooleanLiteral True),
                LabeledExpression
                  (Identifier "int")
                  (LiteralExpression $ IntegerLiteral Dec "32"),
                LabeledExpression
                  (Identifier "unary")
                  ( UnaryExpression
                      Not
                      (LiteralExpression $ BooleanLiteral True)
                  ),
                LabeledExpression
                  (Identifier "binary")
                  ( BinaryExpression
                      Plus
                      (LiteralExpression $ IntegerLiteral Dec "2")
                      (LiteralExpression $ IntegerLiteral Dec "3")
                  )
              ]
          )
