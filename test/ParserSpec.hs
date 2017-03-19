
module ParserSpec where

import Test.Hspec
import Parser.Parser
import Parser.Syntax
import Parser.Lexer (Parser)
import Text.Parsec



parse' :: Parser a -> String -> a
parse' p s = case parse p ""  s of
    Left e -> error $ show e
    Right v -> v

spec :: Spec
spec = do
  describe "parsers" $ do
    it "parses a boolean literal 1" $
      parse' pLiteral "true" `shouldBe` (BooleanLiteral True)
    it "parses a boolean literal 2" $
      parse' pLiteral "false" `shouldBe` (BooleanLiteral False)
    it "parses a null literal" $ 
      parse' pLiteral  "null" `shouldBe` NullLiteral
    it "parses a string literal" $ 
      parse' pLiteral "\"foo\"" `shouldBe` StringLiteral "foo"

    it "parses unary expression not" $ 
      parse' pUnaryExpression "!foo" `shouldBe` (UnaryExpression UnaryNot $ UnaryPostfix $ PostfixLHS $ LHSExpressionNew $ NewExpressionMember $ MemberExpressionPrimary $ IdentifierExp "foo" )
    it "parses multiplicative *" $ do
      let e1 = MultUnary $ UnaryPostfix $ PostfixLHS $ LHSExpressionNew $ NewExpressionMember $ MemberExpressionPrimary $ LiteralExp  $ NumericLiteral $ DecLiteral 2
      let e2 = MultUnary $ UnaryPostfix $ PostfixLHS $ LHSExpressionNew $ NewExpressionMember $ MemberExpressionPrimary $ LiteralExp  $ NumericLiteral $ DecLiteral 3
  
      parse' pMultiplicativeExpression "2 * 3" `shouldBe` MultiplicativeExpression e1 MultTimes e2
    {-it "parses property names" $ -}
      {-parse' pPropertyName ""-}
    

      


