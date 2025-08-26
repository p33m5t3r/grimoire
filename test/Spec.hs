module Main where

import Test.Hspec
import ParserSpec
import TemplateSpec

main :: IO ()
main = hspec $ do
  describe "Parser" ParserSpec.spec
  describe "Template" TemplateSpec.spec