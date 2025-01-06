{-# LANGUAGE LambdaCase #-}
module Templating where
import qualified Data.Map as Map
import Parser
import Data.Char (isLetter)

-- template :: String -> Context -> String
-- template t c = t ++ " templated w/ " ++ c


data Template = Literal String
              | Expr [String]   -- path
              | Loop String String Template
              | Block [Template]
              deriving Show

data Context = String String
             | Array [Context]
             | Object (Map.Map String Context)
             deriving Show


template :: String -> Context -> Either String String
template t c = 
    let err' = "failed to finish parsing"
    in case runP templateParser (newInput t) of
        Left err -> Left $ "failed to parse: " ++ show err
        Right (t, rest) -> 
            if src rest == "" 
                then case render t c of
                    Nothing -> Left "failed to render"
                    Just s -> Right s
                else Left err'

parse :: String -> Template
parse = undefined

render :: Template -> Context -> Maybe String
render (Literal s)  _ = Just s
render (Expr path)  c = strLookup path c
render (Loop pth var t) c = case arrLookup [pth] c of
    Nothing -> Nothing
    (Just arr) -> concat <$> mapM (render t . toNamedCtx) arr
        where toNamedCtx o = Object $ Map.singleton var o
render (Block ts) c = concat <$> mapM (`render` c) ts

lookupPath :: [String] -> Context -> Maybe Context
lookupPath [] v = Just v
lookupPath (key:rest) (Object m) = 
    Map.lookup key m >>= lookupPath rest
lookupPath _ _ = Nothing

typedLookup :: [String] -> Context -> (Context -> Maybe a) -> Maybe a
typedLookup pth c f = lookupPath pth c >>= f

strLookup :: [String] -> Context -> Maybe String
strLookup pth c = lookupPath pth c >>= \case
    String s -> Just s
    _ -> Nothing

arrLookup :: [String] -> Context -> Maybe [Context]
arrLookup pth c = lookupPath pth c >>= \case
    Array a -> Just a
    _ -> Nothing

myContext :: Context
myContext = Object $ Map.fromList [
    ("posts", Array [
        Object $ Map.fromList [("contents", String "post 1"), ("title", String "post1")],
        Object $ Map.fromList [("contents", String "post 2"), ("title", String "post2")]
    ]),
    ("title", String "index.html")
  ]

myTemplate :: Template
myTemplate = Block [
    Literal "<div>",
    Loop "posts" "post" (
        Block [
            Literal "<p>",
            Expr ["post", "contents"],
            Literal "</p>"
        ]
    ),
    Literal "</div>"
    ]

myRawTemplate :: String
myRawTemplate = "<div> {% for post in posts %} <p> {{post.contents}} </p> {% endfor %} </div>"

parseTemplate :: String -> Either ParseError (Template, Input)
parseTemplate = runP templateParser . newInput

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

loopParser :: Parser Template
loopParser = do
    (collection, var) <- betweenWS "{%" "%}" forHeader  -- Using betweenWS like exprParser
    template <- templateParser
    betweenWS "{%" "%}" (word "endfor")  -- Match end tag with whitespace handling
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



