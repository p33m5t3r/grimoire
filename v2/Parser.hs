{-# LANGUAGE OverloadedStrings #-}
module Parser where

import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Types

type Parser = Parsec Void Text

-- Parse a complete document
parseDocument :: Text -> Either (ParseErrorBundle Text Void) Document
parseDocument = parse (document <* eof) "input"

-- Main document parser
document :: Parser Document
document = do
  many newline  -- skip leading newlines
  blocks <- block `sepEndBy` (newline >> newline >> many newline)
  many newline  -- consume trailing newlines
  return $ Document (filter (not . isEmptyBlock) blocks)

-- Helper to filter out empty text blocks
isEmptyBlock :: Block -> Bool
isEmptyBlock (TextBlock [""]) = True
isEmptyBlock (TextBlock []) = True
isEmptyBlock _ = False

-- Not using block separator anymore - each block handles its own termination

-- Parse any block type  
block :: Parser Block
block = choice
  [ headerBlock
  , codeBlock
  , imageBlock  
  , mathBlock
  , quoteBlock
  , listBlock
  , htmlBlock
  , footnoteBlock
  , dropdownBlock
  , textBlock  -- fallback
  ]

-- Header block: # Header or ## Header
headerBlock :: Parser Block
headerBlock = do
  hashes <- some (char '#')
  let level = length hashes
  if level > 2 
    then fail "Only 2 header levels supported"
    else do
      char ' '
      content <- T.pack <$> manyTill anySingle (lookAhead (void newline <|> eof))
      return $ HeaderBlock level content

-- Code block: ```lang\ncode\n```
codeBlock :: Parser Block
codeBlock = do
  string "```"
  lang <- optional $ T.pack <$> manyTill anySingle newline
  content <- T.pack <$> manyTill anySingle (string "```")
  return $ CodeBlock lang content

-- Display image: !!![alt](url) or !!![alt](url){width}
imageBlock :: Parser Block
imageBlock = do
  string "!!!"
  char '['
  alt <- T.pack <$> manyTill anySingle (char ']')
  char '('
  url <- T.pack <$> manyTill anySingle (char ')')
  width <- optional $ do
    char '{'
    w <- read <$> some digitChar
    char '}'
    return w
  return $ ImageBlock alt url width

-- Math block: $$\nmath\n$$
mathBlock :: Parser Block
mathBlock = do
  string "$$"
  newline
  content <- T.pack <$> manyTill anySingle (try (string "\n$$"))
  return $ MathBlock content

-- Quote block: > line1\n> line2
quoteBlock :: Parser Block
quoteBlock = do
  firstLine <- quoteLine
  restLines <- many (newline >> quoteLine)
  return $ QuoteBlock (firstLine : restLines)
  where
    quoteLine = do
      char '>'
      char ' '
      line <- T.pack <$> manyTill anySingle (lookAhead (void newline <|> eof))
      return line

-- List block (simplified - just basic items for now)
listBlock :: Parser Block
listBlock = do
  firstItem <- listItem
  restItems <- many (newline >> listItem)
  return $ ListBlock Unordered (firstItem : restItems)
  where
    listItem = do
      string "- "
      text <- T.pack <$> manyTill anySingle (lookAhead (void newline <|> eof))
      return $ ListItem 0 text

-- HTML block: <html>content</html>
htmlBlock :: Parser Block
htmlBlock = do
  string "<html>"
  content <- T.pack <$> manyTill anySingle (string "</html>")
  return $ HtmlBlock content

-- Footnote block: ~~~n\ncontent\n~~~
footnoteBlock :: Parser Block
footnoteBlock = do
  string "~~~"
  num <- read <$> some digitChar
  newline
  -- For now, just parse as text - we'll add recursive parsing later
  content <- T.pack <$> manyTill anySingle (try (newline *> string "~~~"))
  return $ FootnoteBlock num [TextBlock [content]]

-- Dropdown block: :::teaser\ncontent\n:::
dropdownBlock :: Parser Block
dropdownBlock = do
  string ":::"
  teaser <- T.pack <$> manyTill anySingle newline
  -- For now, just parse as text - we'll add recursive parsing later  
  content <- T.pack <$> manyTill anySingle (try (newline *> string ":::"))
  return $ DropdownBlock teaser [TextBlock [content]]

-- Text block (fallback) - collect lines until double newline or eof
textBlock :: Parser Block
textBlock = do
  firstLine <- T.pack <$> manyTill anySingle (lookAhead (void newline <|> eof))
  return $ TextBlock [firstLine]