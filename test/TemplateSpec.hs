{-# LANGUAGE OverloadedStrings #-}
module TemplateSpec where

import Test.Hspec
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as Map

import Template
import CompileError

spec :: Spec
spec = describe "Template" $ do
  describe "parseTemplate" $ do
    it "parses simple literal" $ do
      parseTemplate "Hello World" `shouldBe` 
        Right (Block [Literal "Hello World"])
    
    it "parses simple expression" $ do
      parseTemplate "{{title}}" `shouldBe`
        Right (Block [Expr ["title"]])
    
    it "parses literal + expression" $ do  
      parseTemplate "Hello {{name}}!" `shouldBe`
        Right (Block [Literal "Hello ", Expr ["name"], Literal "!"])
    
    it "parses nested path expression" $ do
      parseTemplate "{{post.title}}" `shouldBe`
        Right (Block [Expr ["post", "title"]])
    
    it "parses simple for loop" $ do
      parseTemplate "{% for item in items %}{{item}}{% endfor %}" `shouldBe`
        Right (Block [Loop ["items"] "item" (Block [Expr ["item"]])])
    
    it "parses for loop with literal" $ do
      parseTemplate "{% for tag in tags %}[{{tag}}]{% endfor %}" `shouldBe`
        Right (Block [Loop ["tags"] "tag" (Block [Literal "[", Expr ["tag"], Literal "]"])])

  describe "renderTemplate" $ do
    it "renders simple literal" $ do
      let template = Block [Literal "Hello World"]
      renderTemplate template (String "ignored") `shouldBe`
        Right "Hello World"
    
    it "renders simple expression" $ do  
      let template = Block [Expr ["title"]]
          context = Object $ Map.fromList [("title", String "Test")]
      renderTemplate template context `shouldBe`
        Right "Test"
    
    it "renders nested expression" $ do
      let template = Block [Expr ["post", "title"]]  
          context = Object $ Map.fromList 
            [("post", Object $ Map.fromList [("title", String "My Post")])]
      renderTemplate template context `shouldBe`
        Right "My Post"
    
    it "renders simple for loop" $ do
      let template = Block [Loop ["tags"] "tag" (Block [Expr ["tag"]])]
          context = Object $ Map.fromList 
            [("tags", Array [String "haskell", String "test"])]
      renderTemplate template context `shouldBe`
        Right "haskelltest"
    
    it "renders for loop with brackets" $ do
      let template = Block [Loop ["tags"] "tag" (Block [Literal "[", Expr ["tag"], Literal "]"])]
          context = Object $ Map.fromList 
            [("tags", Array [String "haskell", String "test"])]
      renderTemplate template context `shouldBe`
        Right "[haskell][test]"