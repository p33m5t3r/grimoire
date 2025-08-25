{-# LANGUAGE OverloadedStrings #-}
module Types where

import Data.Text (Text)

-- Phase 1: Block-level structure
data Block
  = TextBlock [Text]              -- lines to be parsed for inline content
  | HeaderBlock Int Text          -- level, content
  | CodeBlock (Maybe Text) Text   -- language, source
  | ImageBlock Text Text (Maybe Int)  -- alt, url, width
  | MathBlock Text                -- latex source
  | QuoteBlock [Text]             -- lines (will parse inline content)
  | ListBlock ListType [ListItem] -- type and items
  | HtmlBlock Text                -- raw html
  | FootnoteBlock Int [Block]     -- number, content (recursive!)
  | DropdownBlock Text [Block]    -- teaser, content (recursive!)
  deriving (Show, Eq)

data ListType = Unordered | Ordered
  deriving (Show, Eq)

data ListItem = ListItem 
  { itemLevel :: Int    -- nesting level (0, 1, 2...)
  , itemText :: Text    -- will be parsed for inline content
  } deriving (Show, Eq)

-- Phase 2: Inline elements
data Inline
  = Span Text [Format]            -- text with applied formats
  | InlineCode Text               -- no formatting inside
  | InlineMath Text               -- no formatting inside
  | Link LinkType Text [Inline]   -- type, url, link text
  | InlineImage Text Text         -- alt, url  
  | FootnoteRef Int
  deriving (Show, Eq)

data Format = Bold | Italic | Color Text
  deriving (Show, Eq)

data LinkType = External | Internal
  deriving (Show, Eq)

-- Final document
data Document = Document [Block]
  deriving (Show, Eq)