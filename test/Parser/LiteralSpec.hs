module Parser.LiteralSpec where

import Parser.Literal
import Syntax.Literal
import Test.Hspec
import Text.Parsec

spec :: Spec
spec = do
  describe "integer" $ do
    it "should parse decimal integers" $ do
      parse decimalLiteral "" "433" `shouldBe` Right (DecimalLiteral "433")
      parse decimalLiteral "" "4_3_3" `shouldBe` Right (DecimalLiteral "433")

    it "should parse binary integers" $ do
      parse binaryLiteral "" "0b0110" `shouldBe` Right (BinaryLiteral "0110")
      parse binaryLiteral "" "0b_01_10" `shouldBe` Right (BinaryLiteral "0110")

    it "should parse octal integers" $ do
      parse octalLiteral "" "0o7134" `shouldBe` Right (OctalLiteral "7134")
      parse octalLiteral "" "0o_72_31" `shouldBe` Right (OctalLiteral "7231")

    it "should parse hex integers" $ do
      parse hexLiteral "" "0xf9ab" `shouldBe` Right (HexLiteral "f9ab")
      parse hexLiteral "" "0x_f9_ab" `shouldBe` Right (HexLiteral "f9ab")
