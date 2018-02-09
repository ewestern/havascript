
module ParserSpec where

import Test.Hspec
import Parser.Parser
import Parser.Syntax
import Parser.Lexer (Parser)
import Text.Parsec
import Data.Monoid ((<>))



-- <* eof
parse' :: Parser a -> String -> a
parse' p s = case parse (p <* eof) ""  s of
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
    it "parses a conditional" $ do
      let conditional = IfStatement [AssignmentExpression (ConditionalExp (OperatorExpressionBinary (OperatorExpressionLHS (LHSExpressionNew (NewExpressionMember (MemberExpressionPrimary (IdentifierExp "n"))))) RelLTE (OperatorExpressionLHS (LHSExpressionNew (NewExpressionMember (MemberExpressionPrimary (LiteralExp (NumericLiteral (DecLiteral 1.0)))))))))] (BlockStmt [ReturnStmt (Just [AssignmentExpression (ConditionalExp (OperatorExpressionLHS (LHSExpressionNew (NewExpressionMember (MemberExpressionPrimary (LiteralExp (NumericLiteral (DecLiteral 1.0))))))))])]) (Just (BlockStmt [ReturnStmt (Just [AssignmentExpression (ConditionalExp (OperatorExpressionLHS (LHSExpressionNew (NewExpressionMember (MemberExpressionPrimary (LiteralExp (NumericLiteral (DecLiteral 5.0))))))))])]))
      let block = [ "if (n <= 1) {",
                    "   return 1;",
                    "} else { ",
                    "   return 5;",
                    "}"]
      (parse' pIfStatement $ unlines block) `shouldBe` conditional

    it "parses a function call" $ do
      let exp = CallExpressionMember (MemberExpressionPrimary (IdentifierExp "foo")) [AssignmentExpression (ConditionalExp (OperatorExpressionLHS (LHSExpressionNew (NewExpressionMember (MemberExpressionPrimary (LiteralExp (NumericLiteral (DecLiteral 123.0))))))))]
      parse' pCallExpression "foo(123)" `shouldBe` exp

    it "parses another function call" $ do
      let ast = CallExpressionMember (MemberExpressionPrimary (IdentifierExp "foo")) [AssignmentExpression (ConditionalExp (OperatorExpressionBinary (OperatorExpressionLHS (LHSExpressionNew (NewExpressionMember (MemberExpressionPrimary (IdentifierExp "n"))))) AddPlus (OperatorExpressionLHS (LHSExpressionNew (NewExpressionMember (MemberExpressionPrimary (LiteralExp (NumericLiteral (DecLiteral 1.0)))))))))]
      parse' pCallExpression "foo(n + 1)" `shouldBe` ast

    it "parses a function declaration" $ do
      let ast = FunctionDeclaration "fib" (Just ["n"]) (FunctionBody (Just [SourceElement (IfStmt (IfStatement [AssignmentExpression (ConditionalExp (OperatorExpressionBinary (OperatorExpressionLHS (LHSExpressionNew (NewExpressionMember (MemberExpressionPrimary (IdentifierExp "n"))))) RelLTE (OperatorExpressionLHS (LHSExpressionNew (NewExpressionMember (MemberExpressionPrimary (LiteralExp (NumericLiteral (DecLiteral 1.0)))))))))] (BlockStmt [ReturnStmt (Just [AssignmentExpression (ConditionalExp (OperatorExpressionLHS (LHSExpressionNew (NewExpressionMember (MemberExpressionPrimary (LiteralExp (NumericLiteral (DecLiteral 1.0))))))))])]) (Just (ReturnStmt (Just [AssignmentExpression (ConditionalExp (OperatorExpressionBinary (OperatorExpressionLHS (LHSExpressionCall (CallExpressionMember (MemberExpressionPrimary (IdentifierExp "fib")) [AssignmentExpression (ConditionalExp (OperatorExpressionBinary (OperatorExpressionLHS (LHSExpressionNew (NewExpressionMember (MemberExpressionPrimary (IdentifierExp "n"))))) AddMinus (OperatorExpressionLHS (LHSExpressionNew (NewExpressionMember (MemberExpressionPrimary (LiteralExp (NumericLiteral (DecLiteral 1.0)))))))))]))) AddPlus (OperatorExpressionLHS (LHSExpressionCall (CallExpressionMember (MemberExpressionPrimary (IdentifierExp "fib")) [AssignmentExpression (ConditionalExp (OperatorExpressionBinary (OperatorExpressionLHS (LHSExpressionNew (NewExpressionMember (MemberExpressionPrimary (IdentifierExp "n"))))) AddMinus (OperatorExpressionLHS (LHSExpressionNew (NewExpressionMember (MemberExpressionPrimary (LiteralExp (NumericLiteral (DecLiteral 2.0)))))))))])))))])))))]))
      let fb =   [  "if (n <= 1) {",
                    "   return 1;",
                    "}",
                    "return fib (n - 1) + fib(n - 2);"]
      let prog = unlines $ ["function fib(n) {"] <> fb <> ["}"]
      parse' pFunctionDeclaration prog `shouldBe` ast

    

    

      


