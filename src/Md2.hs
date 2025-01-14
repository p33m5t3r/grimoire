{-# LANGUAGE LambdaCase #-}
module Md2 where
import qualified Control.Applicative as A
import Data.Char (isDigit, isAlphaNum)
import Data.Functor (($>))
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import Control.Monad (MonadPlus(..), void, when)
import Control.Monad.Trans.State  -- for StateT
import Control.Monad.Trans.Class  -- for lift
import Data.Bifunctor (first)
import Parser

testDocument :: String
testDocument = 
             -- header blocks
               "# h1 \n\n"
             ++" ##h2\n\n"
             -- text blocks
             ++"para1word1\npara1word2\n\n"
             ++"para2word1\npara2word2\n\n"
             ++"footnote reference[^1]\n\n"
             ++"plain *bold* _italic_ <pink> colored </pink>\n\n"
             ++"inline code: \n`f::a->b`\n\n"
             ++"inline math: $x^2$\n\n"
             ++"escaped specials: \\` \\$ \\* \\_ \n\n"
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
             ++"1. ol1 2. ol2\n\n"  -- TODO
             -- html block      (can contain double newlines?)
             ++"<html>html contents</html>\n\n"
             -- footnote block  (can contain double newlines)
             ++"[^1]: fnl1\n\nfnl2 :[^1]\n\n" 
             -- dropdown block  (can contain newlines)
             ++".[ dropdown teaser\n dd l1\n\ndd l2\n.]\n\n"
             -- TODO: math block

-- obviously type families/datakinds would be better
-- but this is a first pass and this is easy boilerplate
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

getBlockContent :: MarkdownBlock -> String
getBlockContent (HeaderBlock content)   = content
getBlockContent (TextBlock content)     = content
getBlockContent (ImageBlock content)    = content
getBlockContent (CodeBlock content)     = content
getBlockContent (QuoteBlock content)    = content
getBlockContent (ListBlock content)     = content
getBlockContent (HtmlBlock content)     = content
getBlockContent (FootnoteBlock content) = content
getBlockContent (DropdownBlock content) = content

blockToParser :: MarkdownBlock -> Parser MarkdownItem
blockToParser block = case block of
    HeaderBlock content  -> headerP 
    TextBlock content   -> paragraphP
    ImageBlock content  -> imageP
    CodeBlock content   -> codeP
    QuoteBlock content  -> quoteP 
    ListBlock content   -> listP
    HtmlBlock content   -> htmlP
    FootnoteBlock content -> footnoteP
    DropdownBlock content -> dropdownP

data MarkdownItem
    = Text          String [TextFormat]
    | Header        String Int
    | Image         String String (Maybe Int)   -- alt, url, size
    | Link          String String Bool          -- txt, url, isExternal?
    | Code          String (Maybe String) 
    | Math          String Bool                 -- src, isInline?
    | Html          String
    | FootnoteRef   Int
    | FootnoteDef   [MarkdownItem] Int         -- contents, number
    | Dropdown      String [MarkdownItem]      -- teaser, contents
    | Quote         MarkdownItem                -- for formatting in quotes
    | List          [(Int, String)]
    | Paragraph     [MarkdownItem]             -- a bunch of text
    deriving (Show, Eq)

data TextFormat 
    = Bold 
    | Italic
    | Color String
    | InlineCode
    deriving (Show, Eq)


blockP :: Parser String -> String -> Parser String 
blockP start end = ws *> start <> takeUntilP end

digitP :: Parser Char
digitP = cond isDigit

natP :: Parser String
natP = some digitP

eofP :: Parser () -> Parser ()
eofP p = Parser $ \i -> case src i of
    [] -> Right ((), i)
    _  -> runP p i

optionalP :: Parser a -> Parser (Maybe a)
optionalP p = fmap Just p <|> pure Nothing

tryP :: Parser a -> Parser a
tryP (Parser p) = Parser $ \s -> case p s of
    Left err -> Left err
    Right x  -> Right x

manyTillP :: Parser a -> Parser b -> Parser [a]
manyTillP p end = (end $> []) <|> 
    ((:) <$> p <*> manyTillP p end)

takeUntilP :: String -> Parser String
takeUntilP str = manyTillP char (tryP $ word str)

-- parse out the (mostly) raw contents of the block
-- if we don't do this, managing newlines in each parser is painful
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

-- there should be no whitespace at the start of every block
markdownBlockP :: Parser [MarkdownBlock]
markdownBlockP = some $ foldr (<|>) (fail "chunking failed") parsers
    where parsers = [ headerBlockP, textBlockP, imageBlockP
                    , codeBlockP, quoteBlockP, listBlockP
                    , htmlBlockP, footnoteBlockP, dropdownBlockP
                    ]

headerP :: Parser MarkdownItem
headerP = do
    hashes  <- some (one '#')
    _       <- ws
    txt     <- many char
    return $ Header txt (length hashes)

imageP :: Parser MarkdownItem
imageP = do
    _ <- word "!["
    alt <- takeUntilP "]"
    _ <- one '('
    url <- takeUntilP ")"
    size <- optionalSizeP
    -- might have {invalidnumber} leftover in src, prob ok
    let sizeInt = fmap fromIntegral size
    return $ Image alt url sizeInt
  where optionalSizeP 
         = (Just . read <$> (one '{' *> natP <* one '}')) 
         <|> pure Nothing


codeP :: Parser MarkdownItem
codeP = do
    _ <- word "```"
    lang' <- takeUntilP "\n"
    let lang = if lang' == ""
               then Nothing
               else Just lang'
    src <- some char
    return $ Code src lang

quoteP :: Parser MarkdownItem
quoteP = Quote <$> (one '>' *> ws *> paragraphP)

-- kind of hacky
listP :: Parser MarkdownItem
listP = List <$> many listItemP
  where withNewlineP (Parser p) = Parser $ 
            \s -> p $ Input (src s ++ "\n") (pos s)
        listItemP = do
            _ <- many $ one ' '
            tabs <- many (one '\t')
            _ <- word "- "
            content <- withNewlineP $ takeUntilP "\n"
            return (length tabs, content)

htmlP :: Parser MarkdownItem
htmlP = Html <$> (word "<html>" *> many char)

             
--"[^1]: fnl1\n\nfnl2 :[^1]\n\n" 
-- returns zero as number on failed read
footnoteP :: Parser MarkdownItem
footnoteP = do
    numStr <- natP
    let num = fromMaybe 0 (readMaybe numStr)
    _ <- ws
    contents <- markdownP
    return $ FootnoteDef contents num

dropdownP :: Parser MarkdownItem
dropdownP = undefined


type TextState = ([TextFormat], Bool)
                --[formatting], isEscaped?

type FormatParser = StateT TextState Parser
-- newtype Parser a = Parser { runP :: Input -> Either ParseError (a, Input) }

emptyTextState :: TextState
emptyTextState = ([], False)

getFormats :: FormatParser [TextFormat]
getFormats = gets fst

isEscaped :: FormatParser Bool
isEscaped = gets snd

setEscaped :: Bool -> FormatParser ()
setEscaped b = modify $ \(fmt, _) -> (fmt, b)

pushFormat :: TextFormat -> FormatParser ()
pushFormat f = modify $ first (f :)

popFormat :: FormatParser ()
popFormat = modify $ \(f:fs, e) -> (fs, e)

updateEscaped :: Char -> FormatParser Bool
updateEscaped c = do
    escaped <- isEscaped
    when     escaped $ setEscaped False
    when (c == '\\') $ setEscaped True
    isEscaped

textCharP :: FormatParser Char
textCharP = do
    c <- lift char
    escaped <- updateEscaped c
    if escaped then pure c
    else getFormats >>= setFormat c
  where 
    markers = [('*', Bold), ('_', Italic), ('`', InlineCode)]
    setFormat :: Char -> [TextFormat] -> FormatParser Char
    setFormat c fs = case (lookup c markers, fs) of
      (Just fmt, f:_) | f == fmt -> popFormat >> lift empty
      (Just fmt, _)              -> pushFormat fmt >> lift empty
      _                          -> pure c

instance MonadPlus Parser where
    mzero = empty
    mplus = (<|>)

textP :: FormatParser String
textP = many textCharP

runFmtP :: FormatParser String -> Parser MarkdownItem
runFmtP p = do
    (parsed, state) <- runStateT p emptyTextState
    return $ Text parsed (fst state)

paragraphP :: Parser MarkdownItem
paragraphP = linkP 
         <|> imageP      


footnoteRefP :: Parser MarkdownItem
footnoteRefP = undefined

-- "links: [ext-link](url) [int-link](@url)\n\n"
linkP :: Parser MarkdownItem
linkP = do
    alt <- altTextP
    (url, isInternal) <- urlP
    return $ Link alt url isInternal
 where altTextP = one '[' *> anyCharTill "]"
       urlP = do
        _ <- one '('
        a <- optionalP (one '@') <|> pure Nothing
        let isInternal' = case a of
                (Just _) -> True
                _      -> False
        url <- anyCharTill ")"
        return (url, isInternal')


-- TODO: parse items from blocks
markdownP :: Parser [MarkdownItem]
markdownP = undefined



testStateParser :: FormatParser Char 
testStateParser = do
    pushFormat Bold
    lift empty


x = runP (runFmtP (many testStateParser)) $ newInput ""




test1 :: FormatParser String
test1 = do
    pushFormat Bold
    pure ""

-- Test if lift empty preserves state
test2 :: FormatParser String
test2 = do
    pushFormat Bold
    x <- pure "hello"
    lift empty
    pure x

