{-# LANGUAGE LambdaCase #-}
module Templating 
    ( Template(..)
    , Context(..)
    , parseTemplate
    , renderTemplate
    ) where

import qualified Data.Map as Map
import Data.Char (isLetter)

import Parser 
    ( Parser(..)
    , ParseError
    , Input(..)
    , newInput
    , (<|>), many, some, word, ws, one
    , cond, sepBy, char, betweenWS)


data Template = Literal String
              | Expr [String]   -- path
              | Loop String String Template
              | Block [Template]
              deriving (Show, Eq)

data Context = String String
             | Array [Context]
             | Object (Map.Map String Context)
             deriving Show

-- Parsing/Rendering ==================================================
renderTemplate :: Template -> Context -> Either String String
renderTemplate (Literal s)  _ = Right s
renderTemplate (Expr path)  c = strLookup path c
renderTemplate (Loop pth var t) c = case arrLookup [pth] c of
    Left err -> Left err
    Right arr -> concat <$> mapM (renderTemplate t . toNamedCtx) arr
        where toNamedCtx o = Object $ Map.singleton var o
renderTemplate (Block ts) c = concat <$> mapM (`renderTemplate` c) ts

parseTemplate :: String -> Either ParseError (Template, Input)
parseTemplate = runP templateParser . newInput

-- Context Lookups ====================================================
-- look for values in a context
lookupPath :: [String] -> Context -> Either String Context
lookupPath [] v = Right v
lookupPath (key:rest) (Object m) = 
    case Map.lookup key m of
        Nothing -> Left $ "'" ++ key ++ "' not found in ctx"
        Just v -> lookupPath rest v
lookupPath p c = Left $ "expected object, got: " 
                        ++ show c ++ "at: " ++ show p

typedLookup :: [String] -> Context -> (Context -> Either String a)
            -> Either String a
typedLookup pth c f = lookupPath pth c >>= f

strLookup :: [String] -> Context -> Either String String
strLookup pth c = typedLookup pth c $ \case
    String s -> Right s 
    other -> Left $ "expected string, got: " ++ show other

arrLookup :: [String] -> Context -> Either String [Context]
arrLookup pth c = typedLookup pth c $ \case
    Array a -> Right a
    other -> Left $ "expected array, got: " ++ show other

-- Parsing ==========================================================
templateParser :: Parser Template
templateParser = Block <$> many templatePart

templatePart :: Parser Template
templatePart = literalParser 
          <|> exprParser
          <|> loopParser

-- Parse literal text until we hit {{ or {%
literalParser :: Parser Template
literalParser = Literal <$> some notTagStart
  where
    notTagStart = char >>= \c -> 
      Parser $ \i -> case c of
        '{' -> case src i of
          ('{':_) -> Left ("tag start", pos i)
          ('%':_) -> Left ("tag start", pos i)
          _       -> Right (c, i)
        _   -> Right (c, i)

-- Parse {{path.to.value}} expressions
exprParser :: Parser Template
exprParser = Expr <$> betweenWS "{{" "}}" pathParser
  where
    pathParser = sepBy (some $ cond isValidIdChar) (one '.')
    isValidIdChar c = isLetter c || c == '_'

-- Parse {% for x in y %} expressions
loopParser :: Parser Template
loopParser = do
    (collection, var) <- betweenWS "{%" "%}" forHeader
    template <- templateParser
    _ <- betweenWS "{%" "%}" (word "endfor")
    return $ Loop var collection template
  where
    forHeader = (,)
        <$  word "for" <*  ws
        <*> some (cond isLetter)
        <*  ws <*  word "in" <*  ws
        <*> some (cond isLetter)



