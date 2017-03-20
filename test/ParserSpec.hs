
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

    it "parses prefix expression not" $ 
      parse' pOperatorExpression "!foo" `shouldBe` (OperatorExpressionPrefix PrefixNot $ OperatorExpressionLHS $ LHSExpressionNew $ NewExpressionMember $ MemberExpressionPrimary $ IdentifierExp "foo" )
    
    it "parses multiplicative *" $ do
      let e1 = OperatorExpressionLHS $ LHSExpressionNew $ NewExpressionMember $ MemberExpressionPrimary $ LiteralExp  $ NumericLiteral $ DecLiteral 2
      let e2 = OperatorExpressionLHS $ LHSExpressionNew $ NewExpressionMember $ MemberExpressionPrimary $ LiteralExp  $ NumericLiteral $ DecLiteral 3
      parse' pOperatorExpression "2 * 3" `shouldBe` OperatorExpressionBinary e1 MultTimes e2
    it "parses a return statement" $ do
      let stmt = ReturnStmt $ Just [AssignmentExpression (ConditionalExp (OperatorExpressionBinary (OperatorExpressionLHS (LHSExpressionNew (NewExpressionMember (MemberExpressionPrimary (IdentifierExp "a"))))) AddPlus (OperatorExpressionLHS (LHSExpressionNew (NewExpressionMember (MemberExpressionPrimary (LiteralExp (NumericLiteral (DecLiteral 3.0)))))))))]
      parse' pStatement "return a + 3;" `shouldBe` stmt
    it "parses a function" $ do
      let dec = FunctionDeclaration "foo" (Just ["a"]) (FunctionBody (Just [SourceElement (ReturnStmt (Just [AssignmentExpression (ConditionalExp (OperatorExpressionBinary (OperatorExpressionLHS (LHSExpressionNew (NewExpressionMember (MemberExpressionPrimary (IdentifierExp "a"))))) AddPlus (OperatorExpressionLHS (LHSExpressionNew (NewExpressionMember (MemberExpressionPrimary (LiteralExp (NumericLiteral (DecLiteral 3.0)))))))))]))]))
      parse' pFunctionDeclaration "function foo(a) { return a + 3; }" `shouldBe` dec
    

    

      


