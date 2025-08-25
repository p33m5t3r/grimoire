{-# LANGUAGE OverloadedStrings #-}
module PostMetadata where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Time (Day)
import Data.Time.Format (parseTimeM, defaultTimeLocale)
import CompileError

-- Concrete metadata instead of Map String String
data PostMetadata = PostMetadata
  { postTitle :: Text
  , postDate :: Maybe Day
  , postTags :: [Text]
  , postSlug :: Text  -- derived from directory name
  , postDescription :: Maybe Text
  } deriving (Show, Eq)

-- Context for template rendering  
data PostContext = PostContext
  { postMeta :: PostMetadata
  , postContent :: Text  -- rendered article HTML
  } deriving (Show, Eq)

-- Convert YAML map to structured metadata
parseMetadata :: Text -> Map Text Text -> Either CompileError PostMetadata  
parseMetadata slug yamlMap = do
  title <- case Map.lookup "title" yamlMap of
    Just t -> Right t
    Nothing -> Left $ MetadataError "Missing required field: title"
  
  date <- case Map.lookup "date" yamlMap of
    Just dateStr -> case parseTimeM True defaultTimeLocale "%Y-%m-%d" (T.unpack dateStr) of
      Just d -> Right (Just d)
      Nothing -> Left $ MetadataError $ "Invalid date format: " <> dateStr <> " (expected YYYY-MM-DD)"
    Nothing -> Right Nothing
  
  let tags = case Map.lookup "tags" yamlMap of
        Just tagStr -> T.split (== ',') tagStr
        Nothing -> []
  
  let description = Map.lookup "description" yamlMap
  
  Right $ PostMetadata
    { postTitle = title
    , postDate = date  
    , postTags = map T.strip tags
    , postSlug = slug
    , postDescription = description
    }