module Parser where
import Control.Applicative (many, Alternative, empty, (<|>))
import Data.Char (isAlphaNum)


type ErrorMsg   = String
type LineNo     = Int
type ColNo      = Int
type Position   = (LineNo, ColNo)
type ParseError = (ErrorMsg, Position)

data Input = Input 
    { src :: String
    , pos :: Position 
    } deriving (Show, Eq)

newInput :: String -> Input
newInput s = Input s (0,0)

nextCol :: Position -> Position
nextCol (l, c) = (l, c + 1)

nextLine :: Position -> Position 
nextLine (l, _) = (l + 1, 0)

advance :: Input -> Input
advance (Input (c:cs) p@(line, col)) = 
    Input cs $ case c of
        '\n' -> nextLine p
        _    -> nextCol p
advance (Input [] p) = Input [] p

defaultErr :: ParseError
defaultErr = ("Unspecified Error", (-1, -1))

newtype Parser a = Parser 
    { runP :: Input -> Either ParseError (a, Input) }

instance Semigroup a => Semigroup (Parser a) where
    (Parser p1) <> (Parser p2) = Parser $ \s -> case p1 s of
        Left err -> Left err
        Right (a, s') -> case p2 s' of
            Left err -> Left err
            Right (b, s'') -> Right (a <> b, s'')

instance (Semigroup a, Monoid a) => Monoid (Parser a) where
    mempty = Parser $ \s -> Right (mempty, s)
    mappend = (<>)

instance Functor Parser where
    fmap f (Parser p) = Parser $ \s -> case p s of
        Left err -> Left err
        Right (a, s') -> Right (f a, s')

instance Applicative Parser where
    pure a = Parser $ \s -> Right (a, s)
    (Parser p1) <*> (Parser p2) = Parser $ \s -> case p1 s of
        Left err -> Left err
        Right (f, s') -> case p2 s' of
            Left err -> Left err
            Right (x, s'') -> Right (f x, s'')

instance Alternative Parser where
    empty  = Parser $ \_ -> Left defaultErr
    (Parser p1) <|> (Parser p2) = Parser $ \s -> case p1 s of
        Right x -> Right x
        Left _ -> p2 s

instance Monad Parser where
    return = pure
    (Parser p1) >>= f = Parser $ \s -> case p1 s of
        Left err -> Left err
        Right (a, s') -> case runP (f a) s' of
            Left err -> Left err
            Right (b, s'') -> Right (b, s'')


-- =========== parsing utils ===========
char :: Parser Char
char = Parser $ \i -> case src i of
    (c:_)  -> Right (c, advance i)
    _      -> Left ("empty input", pos i)

condErrMsg :: Char -> String
condErrMsg c = "unexpected character: '" ++ [c] ++ "'"
cond :: (Char -> Bool) -> Parser Char
cond p = char >>= \c ->
    Parser $ \i -> if p c
        then Right (c, i)
        else Left  (condErrMsg c, pos i)
        
one :: Char -> Parser Char
one c = cond (== c)

some :: Parser a -> Parser [a]
some p = (:) <$> p <*> many p

ws :: Parser String
ws = many $ cond (`elem` " \t\n")

spaces :: Parser String
spaces = many $ one ' '

alphanum :: Parser String
alphanum = some $ cond isAlphaNum

-- TODO: add proper strings with escapes and unicode

between :: Char -> Char -> Parser a -> Parser a
between c1 c2 p = one c1 *> p <* one c2

betweenWS :: Char -> Char -> Parser a -> Parser a
betweenWS c1 c2 p = one c1 *> ws *> p <* ws <* one c2

delimited :: Char -> Char -> Char -> Parser a -> Parser [a]
delimited open sep close p = betweenWS open close contents
    where 
        sepWs = ws *> one sep *> ws
        contents = (:) <$> p <*> many (sepWs *> p) <|> pure []


-- =========== yaml parser ===========
data YamlValue = YString String
               | YArray  [String]
               | YObject [(String, YamlValue)]
               deriving (Show, Eq)


yamlStrP :: Parser YamlValue
yamlStrP = YString <$> ((spaces *> alphanum) <* ws)

yamlArrP :: Parser YamlValue
yamlArrP = YArray <$> (ws *> delimited '[' ',' ']' alphanum)

yamlKeyP :: Parser String
yamlKeyP = ((ws *> alphanum) <* spaces) <* one ':'

yamlValueP :: Parser YamlValue
yamlValueP = yamlStrP <|> yamlArrP

yamlKeyValueP :: Parser (String, YamlValue)
yamlKeyValueP = (,) <$> yamlKeyP <*> yamlValueP

yamlParser :: Parser YamlValue
yamlParser = YObject <$> some yamlKeyValueP 


-- =========== custom .md variant parser ===========

data TextFormat = None | Bold | Italic | BoldItalic 
    deriving (Show, Eq)
type Color = String
type HeaderLevel = Int
type IsDisplay = Bool
type URL = String
type SizeTag = String
type IsOrdered = Bool

data MarkdownItem
    = Linebreak
    | Plaintext String  TextFormat Color
    | Header    String  HeaderLevel
    | Math      String  IsDisplay
    | Code      String  String IsDisplay    -- src, language, display=code block
    | Image     String  URL SizeTag
    | Link      String  URL
    | Quote     String
    | RawHtml   String
    | FootRef   Int
    | FootDef   Int     [MarkdownItem]
    | Dropdown  String  [MarkdownItem]
    | List      String  [MarkdownItem] IsOrdered
    | Paragraph [MarkdownItem]          -- purely logical

parseMarkdown :: String -> [MarkdownItem]
parseMarkdown = undefined

processMarkdown :: [MarkdownItem] -> [MarkdownItem]
processMarkdown = id        -- chain text into paragraphs, etc

replace :: String -> String -> String -> String
replace [] _ str = str
replace target new str@(x:xs)
    | take (length target) str == target = new ++ drop (length target) str
    | otherwise = x : replace target new xs

fmt :: String -> [String] -> String
fmt = foldl (flip (replace "{}")) 
-- TODO fix unsafe!!!! fails if more vars than {}'s

fmt1 :: String -> String -> String
fmt1 = replace "{}"

renderMarkdown :: MarkdownItem -> String
renderMarkdown m = case m of
    Linebreak               -> ""
    Header s 0              -> tagEnclose "h1" s ++ "<hr><br>"
    Header s 1              -> tagEnclose "h2" s      -- oh u need more? no u dont
    Code src lang True      -> tagEnclose "pre" $ tagEnclose "code" $ highlight src lang
    Code src lang False     -> spanClass "inline-code" $ highlight src lang
    Image alt url size      -> fmt imageTemplate [url, size, alt]
    Link txt url            -> fmt linkTemplate [url, txt]
    _                       -> "<h1> TODO </h1>"
    
    where   tagEnclose t s  = concat ["<", t, ">", s, "</", t, ">"]
            spanClass c s   = "<span class=\"" ++ c ++ "\">" ++ s ++ "</span>"
            imageTemplate   = "<img src=\"{}\" class=\"{}\" alt=\"{}\">"
            linkTemplate    = "<a href=\"{}\">{}</a>"
            highlight s l   = s       -- todo: syntax highlighting

markdownToHtml :: String -> String
markdownToHtml = concatMap renderMarkdown . processMarkdown . parseMarkdown



