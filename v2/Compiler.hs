{-# LANGUAGE OverloadedStrings #-}
module Compiler where

import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Map (Map)
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void

import Types (Document(..))
import Parser (parseDocument)
import Renderer (renderDocument)
import PostMetadata
import CompileError

type Parser = Parsec Void Text

-- Parse a complete .mdx file into metadata and document
parsePost :: Text -> Text -> Either CompileError (PostMetadata, Document)
parsePost slug content = do
  (yamlText, mdxText) <- splitFrontmatter content
  yamlMap <- parseYamlMap yamlText  
  metadata <- parseMetadata slug yamlMap
  document <- case parseDocument mdxText of
    Left bundle -> Left $ ParseError "MDX content" bundle
    Right doc -> Right doc
  Right (metadata, document)

-- Compile a post to complete article HTML (ready for template)
compilePost :: PostMetadata -> Document -> Text  
compilePost _metadata document = renderDocument document

-- Create template context from compiled post
postToContext :: PostMetadata -> Text -> PostContext
postToContext metadata articleHtml = PostContext
  { postMeta = metadata
  , postContent = articleHtml  
  }

-- Split YAML frontmatter from MDX content
splitFrontmatter :: Text -> Either CompileError (Text, Text)
splitFrontmatter content =
  case parse frontmatterParser "frontmatter" content of
    Left bundle -> Left $ ParseError "frontmatter splitting" bundle  
    Right result -> Right result

frontmatterParser :: Parser (Text, Text)
frontmatterParser = do
  yamlContent <- manyTill anySingle (try $ string "\n\n")
  mdxContent <- T.pack <$> many anySingle
  return (T.pack yamlContent, mdxContent)

-- Simple YAML parser for key-value pairs
parseYamlMap :: Text -> Either CompileError (Map Text Text)
parseYamlMap yamlText =
  case parse yamlMapParser "yaml" yamlText of
    Left bundle -> Left $ ParseError "YAML parsing" bundle
    Right yamlMap -> Right yamlMap

yamlMapParser :: Parser (Map Text Text)  
yamlMapParser = do
  pairs <- many yamlPair
  return $ Map.fromList pairs

yamlPair :: Parser (Text, Text)
yamlPair = do
  key <- T.pack <$> manyTill (satisfy (/= ':')) (char ':')
  _ <- many (char ' ')
  value <- T.pack <$> manyTill anySingle (lookAhead (void newline <|> eof))
  _ <- optional newline
  return (T.strip key, T.strip value)