{-# LANGUAGE OverloadedStrings #-}
module Parser 
  ( parseDocument
  , parseInlines
  , Parser
  , Document(..)
  ) where

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
  _ <- many newline  -- skip leading newlines
  blocks <- block `sepEndBy` (try (newline >> newline >> many newline) <|> (newline >> eof >> return []))
  _ <- many newline  -- consume trailing newlines
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

-- INLINE PARSING ================================================================

-- Parse inline elements within a text string
parseInlines :: Text -> Either (ParseErrorBundle Text Void) [Inline]
parseInlines = parse (inlines <* eof) "inline"

-- Main inline parser - handles all inline constructs
inlines :: Parser [Inline]
inlines = many inline

-- Parse a single inline element
inline :: Parser Inline  
inline = choice
  [ try inlineCode  -- high precedence - no internal formatting
  , try inlineMath  -- high precedence - no internal formatting  
  , try link        -- [text](url) or [text](@internal)
  , try colorText   -- {color|text} syntax
  , try boldText
  , try italicText
  , plainSpan       -- fallback - collect plain text
  , singleChar      -- absolute fallback for any single character
  ]

-- Parse a single character as plain text (absolute fallback)
singleChar :: Parser Inline
singleChar = do
  ch <- anySingle
  return $ Span (T.singleton ch) []

-- Parse *bold text* with proper nesting validation
boldText :: Parser Inline
boldText = do
  char '*'
  content <- manyTill inline (char '*')
  return $ Span (inlinesToText content) [Bold]

-- Parse _italic text_ with proper nesting validation  
italicText :: Parser Inline
italicText = do
  char '_'
  content <- manyTill inline (char '_')
  return $ Span (inlinesToText content) [Italic]

-- Parse {color|text} with color parameter extraction
colorText :: Parser Inline
colorText = do
  char '{'
  color <- T.pack <$> manyTill anySingle (char '|')
  content <- manyTill inline (char '}')
  return $ Span (inlinesToText content) [Color color]

-- Parse `inline code` - no internal formatting
inlineCode :: Parser Inline
inlineCode = do
  char '`'
  content <- T.pack <$> manyTill anySingle (char '`')
  return $ InlineCode content

-- Parse $inline math$ - no internal formatting
inlineMath :: Parser Inline
inlineMath = do
  char '$'
  content <- T.pack <$> manyTill anySingle (char '$')
  return $ InlineMath content

-- Parse [link text](url) or [link text](@internal-path)
link :: Parser Inline
link = do
  char '['
  linkText <- manyTill inline (char ']')
  char '('
  linkType <- (char '@' >> return Internal) <|> return External
  url <- T.pack <$> manyTill anySingle (char ')')
  return $ Link linkType url linkText

-- Parse plain text until next special character
plainSpan :: Parser Inline
plainSpan = do
  text <- T.pack <$> some plainChar
  return $ Span text []
  where
    plainChar = satisfy (\c -> not (c `elem` (specialChars :: String)))
    specialChars = "*_`${[\\}]"  -- characters that start special constructs (added ] for links)

-- Helper: Convert inline list back to text (for nested parsing)
inlinesToText :: [Inline] -> Text
inlinesToText inlines = T.concat [getText i | i <- inlines]
  where
    getText (Span text _) = text
    getText (InlineCode text) = text
    getText (InlineMath text) = text
    getText _ = ""  -- other inline types