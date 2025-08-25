{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Types
import Parser
import Renderer

-- Test MDX input
testInput :: T.Text
testInput = T.unlines
  [ "# Main Header"
  , ""
  , "This is a *bold* paragraph with _italic_ and {pink|colored} text."
  , ""
  , "Check out [my blog](https://example.com) or [this post](@/posts/intro)."
  , ""  
  , "```haskell"
  , "main = putStrLn \"hello\""
  , "```"
  , ""
  , "!!![A test image](test.jpg)"
  ]

main :: IO ()
main = do
  putStrLn "Grimoire v2 - Testing renderer..."
  case parseDocument testInput of
    Left err -> putStrLn $ "Parse error: " ++ show err
    Right doc -> do
      putStrLn "Parsed successfully. HTML output:"
      TIO.putStrLn $ renderDocument doc