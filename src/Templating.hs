{-# LANGUAGE LambdaCase #-}
module Templating where
import qualified Data.Map as Map
import Parser
import Data.Char (isLetter)

data Template = Literal String
              | Expr [String]   -- path
              | Loop String String Template
              | Block [Template]
              deriving (Show, Eq)

data Context = String String
             | Array [Context]
             | Object (Map.Map String Context)
             deriving Show

-- fills a template string with values from a context
template :: String -> Context -> Either String String
template t c = 
    let errUnfinished = "failed to finish parsing"
    in case parseTemplate t of
        Left err -> Left $ "failed to parse: " ++ show err
        Right (t, rest) -> 
            if src rest == "" 
                then case render t c of
                    Left err -> Left $ "failed to render: " ++ err
                    Right s -> Right s
                else Left errUnfinished

-- renders a Template into a string from a context
render :: Template -> Context -> Either String String
render (Literal s)  _ = Right s
render (Expr path)  c = strLookup path c
render (Loop pth var t) c = case arrLookup [pth] c of
    Left err -> Left err
    Right arr -> concat <$> mapM (render t . toNamedCtx) arr
        where toNamedCtx o = Object $ Map.singleton var o
render (Block ts) c = concat <$> mapM (`render` c) ts

-- parses a template string into a Template
parseTemplate :: String -> Either ParseError (Template, Input)
parseTemplate = runP templateParser . newInput

-- look for values in a context
lookupPath :: [String] -> Context -> Either String Context
lookupPath [] v = Right v
lookupPath (key:rest) (Object m) = 
    case Map.lookup key m of
        Nothing -> Left $ "'" ++ key ++ "' not found in ctx"
        Just v -> lookupPath rest v
lookupPath p c = Left $ "expected object, got: " ++ show c

typedLookup :: [String] -> Context -> (Context -> Either String a) -> Either String a
typedLookup pth c f = lookupPath pth c >>= f

strLookup :: [String] -> Context -> Either String String
strLookup pth c = typedLookup pth c $ \case
    String s -> Right s 
    other -> Left $ "expected string, got: " ++ show other

arrLookup :: [String] -> Context -> Either String [Context]
arrLookup pth c = typedLookup pth c $ \case
    Array a -> Right a
    other -> Left $ "expected array, got: " ++ show other

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
    betweenWS "{%" "%}" (word "endfor")
    return $ Loop var collection template
  where
    forHeader = do
        word "for"
        ws
        var <- some $ cond isLetter
        ws
        word "in"
        ws
        collection <- some $ cond isLetter
        return (var, collection)



