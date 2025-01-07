module Parser 
    ( Parser(..)
    , ParseError
    , Position
    , Input(..)
    , newInput
    -- Essential parsing primitives
    , char
    , cond
    , one
    , word
    -- Utility parsers
    , ws
    , spaces
    , alphanum
    , some
    , between
    , betweenWS
    , sepBy
    , notChar
    , anyCharTill
    -- Re-exports from Control.Applicative for convenience
    , (<|>)
    , many
    , empty
    ) where

import Control.Applicative (many, Alternative, empty, (<|>))
import Data.Char (isAlphaNum)
import Control.Monad.Fail (MonadFail)

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
        Right (a, s') -> runP (f a) s'

instance MonadFail Parser where
    fail msg = Parser $ \i -> Left (msg, pos i)


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

-- TODO: add proper strings with escapes and unicode
alphanum :: Parser String
alphanum = some $ cond isAlphaNum

word :: String -> Parser String
word = foldr (\ c -> (<*>) ((:) <$> one c)) (pure [])

between :: String -> String -> Parser a -> Parser a
between w1 w2 p = word w1 *> p <* word w2

betweenWS :: String -> String -> Parser a -> Parser a
betweenWS w1 w2 p = word w1 *> ws *> p <* ws <* word w2

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p sep = (:) <$> p <*> many (sep *> p)

notChar :: Char -> Parser Char
notChar c = cond (/= c)

anyCharTill :: String -> Parser String
anyCharTill end = many (notChar (head end)) <* word end



