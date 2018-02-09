module CodegenSpec where

import Test.Hspec
{-import Parser.Parser-}
{-import Parser.Syntax-}
{-import Parser.Lexer (Parser)-}
import Text.Parsec
import Codegen
import Data.Monoid ((<>))


spec :: Spec
spec = do
  describe "codegen" $ do
