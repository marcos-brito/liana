module Parser.BaseSpec where

import Parser.Base
import Syntax.Base
import Test.Hspec
import Text.Parsec

spec :: Spec
spec = do
  describe "identifer" $ do
    it "should parse an identifer" $ do
      parse identifier "" "ident" `shouldBe` Right (Identifier "ident")
    it "should parse if starts with underscore" $ do
      parse identifier "" "_ident" `shouldBe` Right (Identifier "_ident")
    it "should parse if it has numbers" $ do
      parse identifier "" "_ident123" `shouldBe` Right (Identifier "_ident123")
  describe "qualifiedIdentifier" $ do
    it "should parse a qualified identifer" $
      do
        parse qualifiedIdentifier "" "parent.child.ident"
        `shouldBe` Right [Identifier "parent", Identifier "child", Identifier "ident"]
  describe "ty" $ do
    it "should parse a generic type" $
      do
        parse ty "" "[T]" `shouldBe` Right (Generic (Identifier "T") Nothing)
    it "should parse a generic type with kind" $
      do
        parse ty "" "[T]?" `shouldBe` Right (Generic (Identifier "T") (Just Option))
    it "should parse a concrete type" $
      do
        parse ty "" "Mod.Type"
        `shouldBe` Right (Concrete [Identifier "Mod", Identifier "Type"] [] Nothing)
    it "should parse a concrete type with kind" $
      do
        parse ty "" "Mod.Type!"
        `shouldBe` Right (Concrete [Identifier "Mod", Identifier "Type"] [] (Just Result))
    it "should parse a concrete type with type parameters" $
      do
        parse ty "" "Mod.Type[T,[E]]!"
        `shouldBe` Right
          ( Concrete
              [Identifier "Mod", Identifier "Type"]
              [Concrete [Identifier "T"] [] Nothing, Generic (Identifier "E") Nothing]
              (Just Result)
          )
