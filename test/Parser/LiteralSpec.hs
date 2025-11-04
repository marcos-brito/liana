module Parser.LiteralSpec where

import Parser.Literal
import Syntax.Literal
import Test.Hspec
import Text.Parsec
import Prelude hiding (False, True)

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

  describe "bool" $ do
    it "should parse boolean literals" $ do
      parse booleanLiteral "" "true" `shouldBe` Right True
      parse booleanLiteral "" "false" `shouldBe` Right False

  describe "float" $ do
    it "should parse a float with both sides" $ do
      parse floatLiteral "" "31.43"
        `shouldBe` Right
          ( FloatLiteral
              (Just $ DecimalLiteral "31")
              (Just $ DecimalLiteral "43")
              Nothing
          )

    it "should parse a float with just the exponent" $ do
      parse floatLiteral "" "31e12"
        `shouldBe` Right
          ( FloatLiteral
              (Just $ DecimalLiteral "31")
              Nothing
              (Just $ Exponent Nothing (DecimalLiteral "12"))
          )

  it "should parse a float starting with a dot" $ do
    parse floatLiteral "" ".32"
      `shouldBe` Right
        ( FloatLiteral
            Nothing
            (Just $ DecimalLiteral "32")
            Nothing
        )

    parse floatLiteral "" ".32e13"
      `shouldBe` Right
        ( FloatLiteral
            Nothing
            (Just $ DecimalLiteral "32")
            (Just $ Exponent Nothing (DecimalLiteral "13"))
        )

  it "should parse a float with an exponent" $ do
    parse floatLiteral "" "31.43e12"
      `shouldBe` Right
        ( FloatLiteral
            (Just $ DecimalLiteral "31")
            (Just $ DecimalLiteral "43")
            (Just $ Exponent Nothing (DecimalLiteral "12"))
        )

    parse floatLiteral "" "31.43e+12"
      `shouldBe` Right
        ( FloatLiteral
            (Just $ DecimalLiteral "31")
            (Just $ DecimalLiteral "43")
            (Just $ Exponent (Just Pos) (DecimalLiteral "12"))
        )

    parse floatLiteral "" "31.43e-12"
      `shouldBe` Right
        ( FloatLiteral
            (Just $ DecimalLiteral "31")
            (Just $ DecimalLiteral "43")
            (Just $ Exponent (Just Neg) (DecimalLiteral "12"))
        )
