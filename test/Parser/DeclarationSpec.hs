module Parser.DeclarationSpec where

import Parser.Declaration
import Syntax.Base
import Syntax.Declaration
import Syntax.Expression
import Syntax.Literal
import Test.Hspec
import Text.Parsec

spec :: Spec
spec = do
  describe "attributes" $
    do
      it "should parse attribute" $
        do
          parse attributeDeclaration "" "@!attr"
            `shouldBe` Right (AttributeDeclaration AttrTopLevel [Identifier "attr"] Nothing)

          parse attributeDeclaration "" "@attr"
            `shouldBe` Right (AttributeDeclaration AttrRegular [Identifier "attr"] Nothing)

      it "should parse attribute with parameters" $
        do
          parse attributeDeclaration "" "@!attr(int:32,bool:true)"
            `shouldBe` Right
              ( AttributeDeclaration
                  AttrTopLevel
                  [Identifier "attr"]
                  ( Just $
                      LabeledExpressionList
                        [ LabeledExpression
                            (Identifier "int")
                            (LiteralExpression $ IntegerLiteral Dec "32"),
                          LabeledExpression
                            (Identifier "bool")
                            (LiteralExpression $ BooleanLiteral True)
                        ]
                  )
              )

          parse attributeDeclaration "" "@attr(23,false)"
            `shouldBe` Right
              ( AttributeDeclaration
                  AttrRegular
                  [Identifier "attr"]
                  ( Just $
                      RawExpressionList
                        [ LiteralExpression $ IntegerLiteral Dec "23",
                          LiteralExpression $ BooleanLiteral False
                        ]
                  )
              )
