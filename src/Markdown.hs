module Markdown 
    ( MarkdownItem(..)
    , parseMarkdown
    , renderMarkdown
    -- INTERNAL (testing):
    , plaintext, boldText, italicText, inlineCode, displayCode
    , inlineMath, displayMath, header, subheader, link, image
    , quote, footref, footdef, rawHtml, dropdown, paragraph, document
    , TextFormat(..)
    ) where

import Data.Char (isDigit)
import Text.Read (readMaybe)

import Parser


data TextFormat = None | Bold | Italic | BoldItalic 
    deriving (Show, Eq)
type Color = String
type HeaderLevel = Int
type IsDisplay = Bool
type URL = String
type IsOrdered = Bool

data MarkdownItem
    = Linebreak
    | Plaintext String  TextFormat Color    -- color specifiers to be added later
    | Header    String  HeaderLevel         -- only two levels of headers rn
    | Math      String  IsDisplay
    | Code      String  String IsDisplay    -- src, language, display=code block
    | Image     String  URL
    | Link      String  URL
    | Quote     String
    | RawHtml   String
    | FootRef   Int                         -- the numeric reference to a footnote
    | FootDef   Int     [MarkdownItem]      -- the number and contents of the footnote
    | Dropdown  String  [MarkdownItem]      -- the teaser and the contents
    | List      String  [MarkdownItem] IsOrdered
    | Paragraph [MarkdownItem]              -- purely logical, only assigned after parsing is done
    deriving (Show, Eq)


-- Main rendering function
renderMarkdown :: [MarkdownItem] -> String
renderMarkdown = concatMap renderItem

-- Main parsing function
parseMarkdown :: Input -> Either String [MarkdownItem]
parseMarkdown s = case runP document s of
    Left err -> Left $ show err
    Right (md, rest) -> if src rest == "" 
        then Right md 
        else Left $ ".md parsing unfinished: " ++ show rest

-- PARSING
-- more parser combinators
nonEscaped :: Char -> Parser Char
nonEscaped c = notChar '\\' >>= \x -> 
    if x == c then fail "unexpected delimiter"
    else pure x

escapedString :: Char -> Parser String
escapedString delim = concat <$> many (
    (pure <$> nonEscaped delim) <|> 
    ((: []) <$> (one '\\' *> char)))

-- Basic text formatting parsers
boldText :: Parser MarkdownItem
boldText = Plaintext <$> between "*" "*" (escapedString '*') <*> pure Bold <*> pure ""

italicText :: Parser MarkdownItem
italicText = Plaintext <$> between "_" "_" (escapedString '_') <*> pure Italic <*> pure ""


-- Modified plaintext parser that won't consume past potential paragraph breaks
plaintext :: Parser MarkdownItem
plaintext = do
    -- Get first chunk
    first <- textChunk
    -- Get subsequent chunks separated by single newlines, but not double newlines
    rest <- many (try singleNewlineChunk)
    pure $ Plaintext (collapseWhitespace (concat (first:rest))) None ""
  where
    textChunk = some (char >>= \c -> 
        if c `elem` "*_$`#[]<\n" 
        then fail "reserved character"
        else pure c)
    
    -- Only consume a newline if it's not followed by another newline
    singleNewlineChunk = do
        _ <- one '\n'
        n2 <- char
        if n2 == '\n'
            then fail "double newline"  -- Don't consume double newlines
            else (n2:) <$> textChunk    -- Include the char we peeked

-- Try combinator - attempt a parse but backtrack on failure
try :: Parser a -> Parser a
try p = Parser $ \input -> case runP p input of
    Left _ -> Left ("backtrack", pos input)
    Right (a, rest) -> Right (a, rest)

-- Helper for handling whitespace in text
collapseWhitespace :: String -> String
collapseWhitespace = unwords . words

-- code
inlineCode :: Parser MarkdownItem
inlineCode = Code 
    <$> between "```" "```" (escapedString '`')
    <*> pure ""  -- language tag to be added
    <*> pure False

displayCode :: Parser MarkdownItem
displayCode = Code
    <$> (one '\n' *> word "```" *> ws *> (filter (/= '\n') <$> escapedString '`') <* word "```")
    <*> pure ""  -- language tag to be added
    <*> pure True

-- Math blocks
inlineMath :: Parser MarkdownItem
inlineMath = Math
    <$> between "$" "$" (escapedString '$')
    <*> pure False

displayMath :: Parser MarkdownItem
displayMath = Math
    <$> between "$$" "$$" (escapedString '$')
    <*> pure True

-- Headers
header :: Parser MarkdownItem
header = Header
    <$> (one '#' *> spaces *> many (notChar '\n'))
    <*> pure 1

subheader :: Parser MarkdownItem
subheader = Header
    <$> (word "##" *> spaces *> many (notChar '\n'))
    <*> pure 2

-- Links and images
link :: Parser MarkdownItem
link = Link
    <$> between "[" "]" (escapedString ']')
    <*> between "(" ")" (escapedString ')')

image :: Parser MarkdownItem
image = Image
    <$> (word "![" *> anyCharTill "]")
    <*> between "(" ")" (escapedString ')')

-- Quotes
quote :: Parser MarkdownItem
quote = Quote
    <$> (one '>' *> spaces *> many (notChar '\n'))

-- Footnotes
-- Safe number parsing helper
parseNumber :: String -> Int
parseNumber s = case readMaybe s of
    Just n  -> n
    Nothing -> error "Invalid number format"  -- or handle this more gracefully

footref :: Parser MarkdownItem
footref = FootRef
    <$> (word "[^" *> (parseNumber <$> some (cond isDigit)) <* one ']')

footdef :: Parser MarkdownItem
footdef = FootDef
    <$> (word "[^" *> (parseNumber <$> some (cond isDigit)) <* word "]: ")
    <*> ((\x -> [Plaintext x None ""]) <$> many (notChar ':'))
    <*  (word ":[" *> (parseNumber <$> some (cond isDigit)) <* one ']')

-- HTML blocks
rawHtml :: Parser MarkdownItem
rawHtml = RawHtml
    <$> between "<" ">" (escapedString '>')

-- Dropdowns
dropdown :: Parser MarkdownItem
dropdown = Dropdown
    <$> (word ".[" *> many (notChar '\n'))
    <*> ((\x -> [Plaintext x None ""]) <$> (one '\n' *> many (notChar '.')) <* word ".]")

-- Lists (simplified for now)
listItem :: Parser MarkdownItem
listItem = List
    <$> (one '-' *> spaces *> anyCharTill "\n")
    <*> pure []  -- nested items to be added
    <*> pure False

-- Combine all parsers
markdownItem :: Parser MarkdownItem
markdownItem = choice
    [ boldText
    , italicText
    , inlineCode
    , displayCode
    , inlineMath
    , displayMath
    , header
    , subheader
    , link
    , image
    , quote
    , footref
    , footdef
    , rawHtml
    , dropdown
    , listItem
    , plaintext
    ]
    where
        choice = foldr (<|>) (fail "no matching parser")

paragraph :: Parser MarkdownItem
paragraph = do
    items <- sepBy1 markdownItem (many (oneOf " \t"))  -- Don't consume newlines here
    pure $ Paragraph (combineText items)
  where
    combineText :: [MarkdownItem] -> [MarkdownItem]
    combineText [] = []
    combineText (Plaintext s1 fmt1 c1 : Plaintext s2 fmt2 c2 : rest)
        | fmt1 == fmt2 && c1 == c2 = 
            combineText (Plaintext (s1 ++ " " ++ s2) fmt1 c1 : rest)
    combineText (x:xs) = x : combineText xs


document :: Parser [MarkdownItem]
document =
    many (oneOf " \t\n") *>         -- TODO: take this from Parser
    sepBy paragraph (word "\n\n") <*
    many (oneOf " \t\n")

-- TODO: move to parser
oneOf :: String -> Parser Char
oneOf chars = char >>= \c -> 
    if c `elem` chars 
    then pure c 
    else fail $ "expected one of: " ++ chars

-- TODO: move to parser
sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 p sep = (:) <$> p <*> many (sep *> p)

-- RENDERING
-- Individual item renderers
renderItem :: MarkdownItem -> String
renderItem item = case item of
    Linebreak -> "\n"
    Plaintext str fmt color -> renderText str fmt color
    Header str level -> renderHeader str level
    Math str isDisplay -> renderMath str isDisplay
    Code src lang isDisplay -> renderCode src lang isDisplay
    Image alt url -> renderImage alt url
    Link text url -> renderLink text url
    Quote text -> renderQuote text
    RawHtml html -> html  -- Pass through raw HTML
    FootRef n -> renderFootRef n
    FootDef n items -> renderFootDef n items
    Dropdown teaser content -> renderDropdown teaser content
    List text items isOrdered -> renderList text items isOrdered
    Paragraph items -> renderParagraph items

-- Helper functions for text formatting
renderText :: String -> TextFormat -> String -> String
renderText str fmt _ =      -- TODO: add coloring
    let (pre, post) = case fmt of
            None -> ("","")
            Bold -> ("<strong>","</strong>")
            Italic -> ("<em>","</em>")
            BoldItalic -> ("<strong><em>","</em></strong>")
    in pre ++ escapeHtml str ++ post

-- Helper for HTML escaping
escapeHtml :: String -> String
escapeHtml = concatMap escape
  where
    escape c = case c of
        '<'  -> "&lt;"
        '>'  -> "&gt;"
        '&'  -> "&amp;"
        '"'  -> "&quot;"
        '\'' -> "&#39;"
        _    -> [c]

-- Render headers with appropriate level
renderHeader :: String -> HeaderLevel -> String
renderHeader str level = 
    "<h" ++ show level ++ ">" ++ 
    escapeHtml str ++ 
    "</h" ++ show level ++ ">"

-- Render math blocks using KaTeX or MathJax compatible markup
renderMath :: String -> IsDisplay -> String
renderMath str isDisplay =
    if isDisplay
    then "\\[" ++ str ++ "\\]"
    else "\\(" ++ str ++ "\\)"

-- Render code blocks with optional language highlighting
renderCode :: String -> String -> IsDisplay -> String
renderCode src lang isDisplay =
    if isDisplay
    then "<pre><code" ++ langAttr ++ ">" ++ 
         escapeHtml src ++ 
         "</code></pre>"
    else "<code" ++ langAttr ++ ">" ++ 
         escapeHtml src ++ 
         "</code>"
  where
    langAttr = if null lang 
        then "" 
        else " class=\"language-" ++ lang ++ "\""

-- Render images with alt text
renderImage :: String -> URL -> String
renderImage alt url = 
    "<img src=\"" ++ escapeHtml url ++ 
    "\" alt=\"" ++ escapeHtml alt ++ "\">"

-- Render links
renderLink :: String -> URL -> String
renderLink text url = 
    "<a href=\"" ++ escapeHtml url ++ "\">" ++ 
    escapeHtml text ++ 
    "</a>"

-- Render blockquotes
renderQuote :: String -> String
renderQuote text = 
    "<blockquote>" ++ escapeHtml text ++ "</blockquote>"

-- Render footnote references
renderFootRef :: Int -> String
renderFootRef n = 
    "<sup class=\"footnote-ref\"><a href=\"#fn" ++ show n ++ 
    "\">" ++ show n ++ "</a></sup>"

-- Render footnote definitions
renderFootDef :: Int -> [MarkdownItem] -> String
renderFootDef n items = 
    "<div class=\"footnote\" id=\"fn" ++ show n ++ "\">" ++
    "<sup>" ++ show n ++ "</sup> " ++
    concatMap renderItem items ++
    "</div>"

-- Render collapsible sections (dropdowns)
renderDropdown :: String -> [MarkdownItem] -> String
renderDropdown teaser content =
    "<details>" ++
    "<summary>" ++ escapeHtml teaser ++ "</summary>" ++
    concatMap renderItem content ++
    "</details>"

-- Render lists (ordered or unordered)
renderList :: String -> [MarkdownItem] -> IsOrdered -> String
renderList text items isOrdered =
    let tag = if isOrdered then "ol" else "ul"
        itemContent = "<li>" ++ escapeHtml text ++ 
                     concatMap renderItem items ++ "</li>"
    in "<" ++ tag ++ ">" ++ itemContent ++ "</" ++ tag ++ ">"

-- Render paragraphs
renderParagraph :: [MarkdownItem] -> String
renderParagraph items = 
    "<p>" ++ concatMap renderItem items ++ "</p>"


