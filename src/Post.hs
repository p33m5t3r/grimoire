module Post where
import qualified Data.Map as Map
import Templating (Template(..), Context(..), render, parseTemplate)
import Markdown (renderMarkdown, parseMarkdown, MarkdownItem)
import Parser (Parser(..), Input(..), newInput, anyCharTill, ParseError)
import Control.Monad ((>=>))
import Data.Maybe (fromMaybe)

-- TODO: compile a post.md file to post.hmtl with a template
--       compile index.html from a template


myPostSrc :: String
myPostSrc = "title: post title\n\n#this is markdown!\n\n and ur reading it"

myPostTemplate :: String
myPostTemplate = "<html><div> {{contents}} </div></html>"

myParsedTemplate :: Template
myParsedTemplate = case parseTemplate myPostTemplate of
    Left err -> undefined
    Right t -> fst t

myIndexTemplate :: String
myIndexTemplate = "{% for post in posts %} <p> {{post.title}} </p> {% endfor %}"

type PostMetadata = Map.Map String String

compilePost :: String -> Template -> Either String (String, PostMetadata)
compilePost p t = do
    (rawMeta, rest) <- splitPostMeta p
    metadata        <- parseMeta rawMeta
    parsedMd        <- parseMarkdown rest
    html            <- render t $ contextFrom parsedMd metadata
    return (html, metadata)

-- splits a post.md into the raw yaml header and the raw md contents
splitPostMeta :: String -> Either String (String, Input)
splitPostMeta p = case runP (anyCharTill "\n\n") $ newInput p of
    Left err -> Left $ show err
    Right (rawYaml, rest) -> Right (rawYaml, rest)

-- parses raw yaml metadata into PostMetadata
parseMeta :: String -> Either String PostMetadata
parseMeta _ = Right Map.empty

-- derives the rendering context for a post
contextFrom :: [MarkdownItem] -> PostMetadata -> Context
contextFrom doc meta = let contents  = renderMarkdown doc
                           title     = getPostTitle meta
                           footnotes = "todo" in
    Object $ Map.fromList [
        ("contents", String contents),
        ("footnotes", String footnotes),
        ("title", String title)
    ]

-- accessor functions for postMetadata
getPostTitle :: PostMetadata -> String
getPostTitle meta = Data.Maybe.fromMaybe 
                        "untitled" 
                        (Map.lookup "title" meta)


test :: Either String (String, PostMetadata)
test = compilePost myPostSrc myParsedTemplate
