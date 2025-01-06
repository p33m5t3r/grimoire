{-# LANGUAGE LambdaCase #-}
module Templating where
import qualified Data.Map as Map

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


template :: String -> Context -> Maybe String
template t = render (parse t)

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

