{-# LANGUAGE LambdaCase #-}
module Md2 where
import Parser
import Data.Char (isDigit, isAlphaNum)
import Data.Functor (($>))

testDocument :: String
testDocument = 
             -- header blocks
               "# h1 \n\n"
             ++"##h2\n\n"
             -- text blocks
             ++"para1word1\npara1word2\n\n"
             ++"para2word1\npara2word2\n\n"
             ++"footnote reference[^1]\n\n"
             ++"plain *bold* _italic_ <pink> colored </pink>\n\n"
             ++"inline code: \n`f::a->b`\n\n"
             ++"escaped specials: \\` \\$ \\$$$ \\< \\* \\_ \\[ \n\n"
             ++"links: [ext-link](url) [int-link](@url)\n\n"
             ++"inline imgs:\n![img1](url)\n![img2](url)\n\n"
             -- image block
             ++"![standalone image](url){80}\n\n"
             -- code block
             ++"```haskell\ncode block l1\n\n code block l2\n```\n\n"
             -- quote block
             ++"> block quote\n\n"
             -- list blocks
             ++"- ul1 \n- ul2\n\t-uls1\n\n"
             ++"1. ol1 2. ol2\n\n"
             -- html block      (might contain double newlines?)
             ++"<html>html contents</html>\n\n"
             -- footnote block  (can contain double newlines)
             ++"[^1]: fnl1\n\nfnl2 :[^1]\n\n" 
             -- dropdown block  (can contain newlines)
             ++".[ dropdown teaser\n dd l1\n\ndd l2\n.]\n\n"

data MarkdownBlock
    = HeaderBlock   String
    | TextBlock     String
    | ImageBlock    String
    | CodeBlock     String
    | QuoteBlock    String
    | ListBlock     String
    | HtmlBlock     String
    | FootnoteBlock String
    | DropdownBlock String
    deriving (Show, Eq)

data MarkdownItem
    = Text          String TextFormat
    | Header        String Int
    | Image         String String (Maybe Int)   -- alt, url, size
    | Link          String String Bool          -- txt, url, isExternal?
    | Code          String String Bool          -- src, language, isInline?
    | Html          String
    | FootnoteRef   Int
    | FootnoteDef   [MarkdownItem] Int         -- contents, number
    | Dropdown      String [MarkdownItem]      -- teaser, contents
    | Quote         [MarkdownItem]              -- for formatting in quotes
    | List          [MarkdownItem]
    | Paragraph     [MarkdownItem]

data TextFormat 
    = Bold 
    | Italic
    | Color String


blockP :: Parser String -> String -> Parser String 
blockP start end = ws *> start <> takeUntilP end

digitP :: Parser Char
digitP = cond isDigit

natP :: Parser String
natP = some digitP

tryP :: Parser a -> Parser a
tryP (Parser p) = Parser $ \s -> case p s of
    Left err -> Left err
    Right x  -> Right x

manyTillP :: Parser a -> Parser b -> Parser [a]
manyTillP p end = (end $> []) <|> 
    ((:) <$> p <*> manyTillP p end)

takeUntilP :: String -> Parser String
takeUntilP str = manyTillP char (tryP $ word str)

headerBlockP :: Parser MarkdownBlock
headerBlockP = HeaderBlock <$> blockP (some (one '#')) "\n\n"

textBlockP :: Parser MarkdownBlock
textBlockP = TextBlock <$> blockP (pure <$> cond isAlphaNum) "\n\n"

imageBlockP :: Parser MarkdownBlock
imageBlockP = ImageBlock <$> blockP (word "!") "\n\n"

codeBlockP :: Parser MarkdownBlock
codeBlockP = CodeBlock <$> blockP (word "```") "```\n\n"

quoteBlockP :: Parser MarkdownBlock
quoteBlockP = QuoteBlock <$> blockP (word ">") "\n\n"

listBlockP :: Parser MarkdownBlock
listBlockP = ListBlock <$> blockP (word "-" <|> word "1. ") "\n\n"

htmlBlockP :: Parser MarkdownBlock
htmlBlockP = HtmlBlock <$> blockP (word "<html>") "</html>\n\n"

footnoteBlockP :: Parser MarkdownBlock
footnoteBlockP = do
    ws
    word "[^"
    num <- natP     -- variable stop pattern makes this fucky
    word "]:"
    content <- takeUntilP (":[^" ++ num ++ "]\n\n")
    return $ FootnoteBlock (num <> content)

dropdownBlockP :: Parser MarkdownBlock
dropdownBlockP = DropdownBlock <$> blockP (word ".[") ".]\n\n"


markdownBlockP :: Parser [MarkdownBlock]
markdownBlockP = some $ foldr (<|>) (fail "chunking failed") parsers
    where parsers = [ headerBlockP, textBlockP, imageBlockP
                    , codeBlockP, quoteBlockP, listBlockP
                    , htmlBlockP, footnoteBlockP, dropdownBlockP
                    ]


-- TODO: parse items from blocks



renderItem :: MarkdownItem -> String
renderItem = \case
    Text txt fmt                -> renderText txt fmt
    Header txt lvl              -> renderHeader txt lvl
    Image alt url size          -> renderImage alt url size
    Link txt url isExt          -> renderLink txt url isExt
    Code src lang isInline      -> renderCode src lang isInline
    Html content                -> content  -- passthrough
    FootnoteRef n               -> renderFootnoteRef n
    FootnoteDef contents n      -> renderFootnoteDef contents n
    Dropdown teaser contents    -> renderDropdown teaser contents
    Quote contents              -> renderQuote contents
    List items                  -> renderList items
    Paragraph items             -> renderParagraph items

renderText :: String -> TextFormat -> String
renderText txt = \case
    Bold -> "<strong>" ++ txt ++ "</strong>"
    Italic -> "<em>" ++ txt ++ "</em>"
    Color c -> "<span style=\"color: " ++ c ++ "\">" ++ txt ++ "</span>"

renderHeader :: String -> Int -> String
renderHeader txt lvl = "<h" ++ show lvl ++ ">" ++ txt ++ "</h" ++ show lvl ++ ">"

renderImage :: String -> String -> Maybe Int -> String
renderImage alt url size = case size of
    Nothing -> "<img src=\"" ++ url ++ "\" alt=\"" ++ alt ++ "\">"
    Just w -> "<img src=\"" ++ url ++ "\" alt=\"" ++ alt ++ 
              "\" style=\"width: " ++ show w ++ "%\">"

renderLink :: String -> String -> Bool -> String
renderLink txt url isExt = 
    "<a href=\"" ++ url ++ "\"" ++ extAttr ++ ">" ++ txt ++ "</a>"
  where
    extAttr = if isExt then " target=\"_blank\"" else ""

renderCode :: String -> String -> Bool -> String
renderCode src lang isInline = if isInline
    then "<code>" ++ src ++ "</code>"
    else "<pre><code class=\"language-" ++ lang ++ "\">" ++ src ++ "</code></pre>"

renderFootnoteRef :: Int -> String
renderFootnoteRef n = "<sup><a href=\"#fn" ++ show n ++ 
                     "\" class=\"footnote-ref\">" ++ show n ++ "</a></sup>"

renderFootnoteDef :: [MarkdownItem] -> Int -> String
renderFootnoteDef contents n = 
    "<div class=\"footnote\" id=\"fn" ++ show n ++ "\">" ++
    "<sup>" ++ show n ++ "</sup> " ++
    concatMap renderItem contents ++
    "</div>"

renderDropdown :: String -> [MarkdownItem] -> String
renderDropdown teaser contents =
    "<details><summary>" ++ teaser ++ "</summary>" ++
    concatMap renderItem contents ++
    "</details>"

renderQuote :: [MarkdownItem] -> String
renderQuote contents = 
    "<blockquote>" ++ concatMap renderItem contents ++ "</blockquote>"

renderList :: [MarkdownItem] -> String
renderList items = "<ul>" ++ concatMap renderListItem items ++ "</ul>"
  where
    renderListItem item = "<li>" ++ renderItem item ++ "</li>"

renderParagraph :: [MarkdownItem] -> String
renderParagraph items = "<p>" ++ concatMap renderItem items ++ "</p>"

