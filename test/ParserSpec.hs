{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module ParserSpec where

import Test.Hspec
import Data.Text (Text)
import qualified Data.Text as T

import Types
import Parser

spec :: Spec
spec = describe "Parser" $ do
  describe "parseDocument" $ do
    it "parses a simple header" $ do
      parseDocument "# Header 1" `shouldBe` 
        Right (Document [HeaderBlock 1 "Header 1"])
    
    it "parses header with content" $ do  
      let input = "# Main Header\n\nSome text"
      parseDocument input `shouldBe` 
        Right (Document [HeaderBlock 1 "Main Header", TextBlock ["Some text"]])
    
    it "parses code blocks" $ do
      let input = "```haskell\nmain = putStrLn \"hello\"\n```"
      parseDocument input `shouldBe`
        Right (Document [CodeBlock (Just "haskell") "main = putStrLn \"hello\"\n"])
    
    it "parses display images" $ do
      parseDocument "!!![alt text](image.jpg)" `shouldBe`
        Right (Document [ImageBlock "alt text" "image.jpg" Nothing])
        
    it "parses display images with width" $ do  
      parseDocument "!!![alt](image.jpg){80}" `shouldBe`
        Right (Document [ImageBlock "alt" "image.jpg" (Just 80)])
    
    it "parses math blocks" $ do
      let input = "$$\nx^2 + y^2 = z^2\n$$"
      parseDocument input `shouldBe`
        Right (Document [MathBlock "x^2 + y^2 = z^2"])
    
    it "parses quote blocks" $ do
      let input = "> This is a quote\n> Second line"
      parseDocument input `shouldBe` 
        Right (Document [QuoteBlock ["This is a quote", "Second line"]])
    
    it "parses simple lists" $ do
      let input = "- Item 1\n- Item 2"  
      parseDocument input `shouldBe`
        Right (Document [ListBlock Unordered 
          [ListItem 0 "Item 1", ListItem 0 "Item 2"]])
    
    it "parses HTML blocks" $ do
      let input = "<html><p>Raw HTML</p></html>"
      parseDocument input `shouldBe`
        Right (Document [HtmlBlock "<p>Raw HTML</p>"])
    
    it "parses footnote blocks" $ do
      let input = "~~~1\nFootnote content\n~~~"
      parseDocument input `shouldBe`
        Right (Document [FootnoteBlock 1 [TextBlock ["Footnote content"]]])
        
    it "parses dropdown blocks" $ do
      let input = ":::Click here\nHidden content\n:::"
      parseDocument input `shouldBe`
        Right (Document [DropdownBlock "Click here" [TextBlock ["Hidden content"]]])

  describe "headerBlock" $ do  
    it "rejects headers with more than 2 levels" $ do
      parseDocument "### Too many hashes" `shouldSatisfy` 
        \case Left _ -> True; Right _ -> False

    it "requires space after hashes" $ do
      parseDocument "#NoSpace" `shouldSatisfy`
        \case Left _ -> True; Right _ -> False

  describe "multi-block parsing" $ do
    it "parses header followed by text (simple)" $ do
      parseDocument "# Header\n\nText" `shouldBe`
        Right (Document [HeaderBlock 1 "Header", TextBlock ["Text"]])
    
    it "parses two headers" $ do
      parseDocument "# Header 1\n\n## Header 2" `shouldBe`
        Right (Document [HeaderBlock 1 "Header 1", HeaderBlock 2 "Header 2"])
        
    it "parses header then code block" $ do
      let input = "# Header\n\n```\ncode\n```"
      parseDocument input `shouldBe`
        Right (Document [HeaderBlock 1 "Header", CodeBlock (Just "") "code\n"])
        
    it "parses three different blocks" $ do
      let input = "# Header\n\nSome text\n\n!!![image](url)"
      parseDocument input `shouldBe`
        Right (Document 
          [ HeaderBlock 1 "Header"
          , TextBlock ["Some text"] 
          , ImageBlock "image" "url" Nothing
          ])
    
    it "handles trailing newlines" $ do
      parseDocument "# Header\n\nText\n\n" `shouldBe`
        Right (Document [HeaderBlock 1 "Header", TextBlock ["Text"]])
        
    it "handles leading newlines" $ do
      parseDocument "\n\n# Header\n\nText" `shouldBe`
        Right (Document [HeaderBlock 1 "Header", TextBlock ["Text"]])

  describe "inline parsing" $ do
    it "parses plain text" $ do
      parseInlines "Hello world" `shouldBe`
        Right [Span "Hello world" []]
    
    it "parses bold text" $ do
      parseInlines "*bold*" `shouldBe`
        Right [Span "bold" [Bold]]
        
    it "parses italic text" $ do
      parseInlines "_italic_" `shouldBe`
        Right [Span "italic" [Italic]]
        
    it "parses inline code" $ do
      parseInlines "`code`" `shouldBe`
        Right [InlineCode "code"]
        
    it "parses inline math" $ do  
      parseInlines "$x^2$" `shouldBe`
        Right [InlineMath "x^2"]
        
    it "parses mixed formatting" $ do
      parseInlines "This is *bold* and _italic_ text" `shouldBe`
        Right [Span "This is " [], Span "bold" [Bold], Span " and " [], 
               Span "italic" [Italic], Span " text" []]
               
    it "parses nested formatting" $ do
      parseInlines "*bold _and italic_*" `shouldBe`
        Right [Span "bold and italic" [Bold]]
        -- Note: This is the current implementation - it flattens nesting
        
    it "handles code with no internal formatting" $ do
      parseInlines "`*no bold here*`" `shouldBe`
        Right [InlineCode "*no bold here*"]
        
    it "handles math with no internal formatting" $ do
      parseInlines "$*not bold*$" `shouldBe`
        Right [InlineMath "*not bold*"]
        
    it "parses color text" $ do
      parseInlines "{red|colored text}" `shouldBe`
        Right [Span "colored text" [Color "red"]]
        
    it "parses color with nested formatting" $ do
      parseInlines "{blue|*bold blue*}" `shouldBe`
        Right [Span "bold blue" [Color "blue"]]
        -- Note: Like bold/italic, nesting currently flattens
        
    it "parses mixed colors and formatting" $ do
      parseInlines "This is {pink|pink text} and *bold*" `shouldBe`
        Right [Span "This is " [], Span "pink text" [Color "pink"], 
               Span " and " [], Span "bold" [Bold]]