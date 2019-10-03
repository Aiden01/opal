module Main where

import           Text.Megaparsec
import           Text.Megaparsec.Error
import           Opal.Parsing.Parser
import           System.Environment             ( getArgs )

parseProgram :: String -> IO ()
parseProgram buffer = case parse (program <* eof) "" buffer of
  Left  e   -> putStrLn (errorBundlePretty e)
  Right ast -> print ast

main :: IO ()
main = getArgs >>= readFile . head >>= parseProgram
