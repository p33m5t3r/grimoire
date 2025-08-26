{-# LANGUAGE OverloadedStrings #-}
module Template where

import Control.Monad (void)
import Data.Char (isLetter)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Time (Day, formatTime, defaultTimeLocale)

import CompileError

-- Template AST (same as v1)
data Template 
  = Literal Text
  | Expr [Text]   -- path like ["posts", "0", "title"]
  | Loop [Text] Text Template  -- collection path, variable, body
  | Block [Template]
  deriving (Show, Eq)

-- Generic context for template rendering (same as v1)  
data Context
  = String Text
  | Array [Context]  
  | Object (Map Text Context)
  deriving (Show, Eq)

-- Bridge typeclass: concrete types -> generic context
class ToContext a where
  toContext :: a -> Context

-- Template parsing and rendering
type Parser = Parsec Void Text

parseTemplate :: Text -> Either CompileError Template
parseTemplate input = 
  case parse templateParser "template" input of
    Left bundle -> Left $ TemplateError $ "Template parse error: " <> T.pack (show bundle)
    Right template -> Right template

renderTemplate :: Template -> Context -> Either CompileError Text
renderTemplate template context = 
  case renderTemplate' template context of
    Left err -> Left $ TemplateError err
    Right result -> Right result

-- Internal rendering (can fail with String errors)
renderTemplate' :: Template -> Context -> Either Text Text
renderTemplate' (Literal text) _ = Right text
renderTemplate' (Expr path) context = strLookup path context  
renderTemplate' (Loop collectionPath var body) context = do
  arr <- arrLookup collectionPath context
  results <- mapM renderItem arr
  Right $ T.concat results
  where
    renderItem item = renderTemplate' body (Object $ Map.singleton var item)
renderTemplate' (Block templates) context = do
  results <- mapM (`renderTemplate'` context) templates  
  Right $ T.concat results

-- Template parser (ported from v1 to megaparsec)
templateParser :: Parser Template
templateParser = Block <$> many templatePart

templatePart :: Parser Template  
templatePart = choice
  [ try exprParser
  , try loopParser  
  , literalParser
  ]

-- Parse literal text until we hit {{ or {%  
literalParser :: Parser Template
literalParser = do
  text <- T.pack <$> many notTagStart  -- Use 'many' instead of 'some'
  if T.null text
    then empty  -- No literal text, let other parsers try
    else return $ Literal text
  where
    notTagStart = do
      c <- lookAhead anySingle
      case c of
        '{' -> do
          -- Check if this is the start of {{ or {%
          ahead <- optional (lookAhead (anySingle >> anySingle))
          case ahead of  
            Just '{' -> empty  -- Stop here, let exprParser handle {{
            Just '%' -> empty  -- Stop here, let loopParser handle {%  
            _ -> anySingle     -- Single { is literal text, consume it
        _ -> anySingle  -- Normal character, consume it

-- Parse {{path.to.value}} expressions
exprParser :: Parser Template
exprParser = do
  _ <- string "{{"
  _ <- many (char ' ')
  path <- pathParser  
  _ <- many (char ' ')
  _ <- string "}}"
  return $ Expr path
  where
    pathParser = sepBy1 identifier (char '.')
    identifier = T.pack <$> some (satisfy isValidIdChar)
    isValidIdChar c = c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z' || c == '_' || c >= '0' && c <= '9'

-- Parse {% for x in y %} ... {% endfor %} 
loopParser :: Parser Template
loopParser = do
  (collection, var) <- forHeader
  body <- templateParser
  _ <- endFor
  return $ Loop collection var body
  where
    forHeader = do
      _ <- string "{%"
      _ <- many (char ' ')
      _ <- string "for"
      _ <- some (char ' ')
      var <- T.pack <$> some (satisfy isLetter)
      _ <- some (char ' ')  
      _ <- string "in"
      _ <- some (char ' ')
      collectionPath <- T.split (== '.') . T.pack <$> some (satisfy (\c -> isLetter c || c == '.'))
      _ <- many (char ' ')
      _ <- string "%}"
      return (collectionPath, var)
    
    endFor = do
      _ <- string "{%"
      _ <- many (char ' ')
      _ <- string "endfor"
      _ <- many (char ' ')  
      _ <- string "%}"
      return ()

-- Context lookups (ported from v1)
lookupPath :: [Text] -> Context -> Either Text Context
lookupPath [] value = Right value
lookupPath (key:rest) (Object m) = 
  case Map.lookup key m of
    Nothing -> Left $ "Key '" <> key <> "' not found in context"
    Just value -> lookupPath rest value
lookupPath path context = Left $ "Expected object for path " <> T.pack (show path) <> ", got: " <> T.pack (show context)

strLookup :: [Text] -> Context -> Either Text Text  
strLookup path context = do
  value <- lookupPath path context
  case value of
    String text -> Right text
    other -> Left $ "Expected string, got: " <> T.pack (show other)

arrLookup :: [Text] -> Context -> Either Text [Context]
arrLookup path context = do  
  value <- lookupPath path context
  case value of
    Array arr -> Right arr
    other -> Left $ "Expected array, got: " <> T.pack (show other)

-- Utility instances for our concrete types
instance ToContext Text where
  toContext = String

instance ToContext Day where
  toContext day = String $ T.pack $ formatTime defaultTimeLocale "%B %d, %Y" day

instance ToContext a => ToContext (Maybe a) where
  toContext Nothing = String ""
  toContext (Just x) = toContext x

instance ToContext a => ToContext [a] where
  toContext xs = Array $ map toContext xs