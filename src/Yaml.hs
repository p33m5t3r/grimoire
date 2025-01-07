module Yaml where
import Parser

-- =========== yaml parser ===========
data YamlValue = YString String
               | YArray  [String]
               | YObject [(String, YamlValue)]
               deriving (Show, Eq)

yamlStrP :: Parser YamlValue
yamlStrP = YString <$> ((spaces *> valueStr) <* ws)
  where
    valueStr = some (char >>= \c ->
        if c == '\n' 
        then fail "newline"
        else pure c)

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

delimited :: Char -> Char -> Char -> Parser a -> Parser [a]
delimited open sep close p = betweenWS' open close contents
    where 
        sepWs = ws *> one sep *> ws
        betweenWS' c1 c2 p' = one c1 *> ws *> p' <* ws <* one c2
        contents = (:) <$> p <*> many (sepWs *> p) <|> pure []



