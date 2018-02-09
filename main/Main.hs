module Main where

import Parser.Parser
import Parser.Syntax (Program(..))
import Text.Parsec

import Control.Monad.Trans
import System.Console.Haskeline

process :: String -> IO ()
process line = do
  let res = parse pProgram "repl" line
  case res of
    Left err -> print err
    Right (Program ex) -> mapM_ print ex

main :: IO ()
main = runInputT defaultSettings loop
  where
  loop = do
    minput <- getInputLine "ready> "
    case minput of
      Nothing -> outputStrLn "Goodbye."
      Just input -> (liftIO $ process input) >> loop
