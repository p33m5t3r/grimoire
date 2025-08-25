{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text as T
import Types
import Parser

-- Test MDX input
testInput :: T.Text
testInput = T.unlines
  [ "# Main Header"
  , ""
  , "This is a paragraph."
  , ""  
  , "```haskell"
  , "main = putStrLn \"hello\""
  , "```"
  , ""
  , "!!![A test image](test.jpg)"
  ]

main :: IO ()
main = do
  putStrLn "Grimoire v2 - Testing parser..."
  case parseDocument testInput of
    Left err -> putStrLn $ "Parse error: " ++ show err
    Right doc -> do
      putStrLn "Parsed successfully:"
      print doc