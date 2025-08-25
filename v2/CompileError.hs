{-# LANGUAGE OverloadedStrings #-}
module CompileError where

import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec (ParseErrorBundle)
import Data.Void (Void)

data CompileError
  = ParseError Text (ParseErrorBundle Text Void)
  | MetadataError Text  
  | TemplateError Text
  | FileError FilePath Text  -- file path, error description
  deriving (Show, Eq)

-- Helper to show errors nicely
displayError :: CompileError -> Text
displayError (ParseError context bundle) = 
  "Parse error in " <> context <> ": " <> T.pack (show bundle)
displayError (MetadataError msg) = 
  "Metadata error: " <> msg
displayError (TemplateError msg) = 
  "Template error: " <> msg  
displayError (FileError path msg) = 
  "File error (" <> T.pack path <> "): " <> msg