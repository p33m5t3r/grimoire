{-# LANGUAGE OverloadedStrings #-}
module Renderer where

import Data.Text (Text)
import qualified Data.Text as T
import Types
import Parser (parseInlines)

renderDocument :: Document -> Text
renderDocument (Document blocks) = 
  T.concat (map renderBlock blocks)

-- Full HTML wrapper (moved from renderDocument for flexibility)
renderFullDocument :: Document -> Text -> Text
renderFullDocument doc title = 
  wrapHtml title $ renderDocument doc

wrapHtml :: Text -> Text -> Text  
wrapHtml title content = T.unlines
  [ "<!DOCTYPE html>"
  , "<html lang=\"en\">"
  , "<head>"
  , "    <meta charset=\"UTF-8\">"
  , "    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">"
  , "    <title>" <> title <> "</title>"
  , "    <link rel=\"stylesheet\" href=\"style.css\">"
  , "</head>"
  , "<body>"
  , "    <div class=\"container\">"
  , "        <article class=\"post-content\">"
  , content
  , "        </article>"
  , "    </div>"
  , "</body>"
  , "</html>"
  ]

renderBlock :: Block -> Text
renderBlock (TextBlock lines) = 
  "<p>" <> T.intercalate " " (map renderInlines lines) <> "</p>\n"
renderBlock (HeaderBlock 1 text) = 
  "<h1>" <> escapeHtml text <> "</h1>\n<hr>\n<br>\n"
renderBlock (HeaderBlock 2 text) = 
  "<h2>" <> escapeHtml text <> "</h2>\n"
renderBlock (HeaderBlock n text) = 
  "<h" <> T.pack (show n) <> ">" <> escapeHtml text <> "</h" <> T.pack (show n) <> ">\n"
renderBlock (CodeBlock maybeLang code) =
  "<pre><code" <> langAttr <> ">" <> escapeHtml code <> "</code></pre>\n"
  where
    langAttr = case maybeLang of
      Just lang -> " class=\"language-" <> lang <> "\""
      Nothing -> ""
renderBlock (ImageBlock alt url maybeWidth) =
  "<p><img class=\"md-img\" src=\"" <> url <> "\" alt=\"" <> alt <> "\"" <> widthAttr <> "></p>\n"
  where
    widthAttr = case maybeWidth of
      Just w -> " width=\"" <> T.pack (show w) <> "\""
      Nothing -> ""
renderBlock (MathBlock math) =
  "<div class=\"math-block\">$$" <> escapeHtml math <> "$$</div>\n"
renderBlock (QuoteBlock lines) =
  "<blockquote>\n" <> T.concat (map (\l -> "  <p>" <> escapeHtml l <> "</p>\n") lines) <> "</blockquote>\n"
renderBlock (ListBlock listType items) =
  let tag = case listType of { Ordered -> "ol"; Unordered -> "ul" }
  in "<" <> tag <> ">\n" <> T.concat (map renderListItem items) <> "</" <> tag <> ">\n"
renderBlock (HtmlBlock html) = html <> "\n"
renderBlock (FootnoteBlock num blocks) =
  "<div id=\"fn" <> T.pack (show num) <> "\" class=\"footnote\">\n" <>
  "  <p><a href=\"#src" <> T.pack (show num) <> "\">[<- " <> T.pack (show num) <> "]</a>\n" <>
  T.concat (map renderBlock blocks) <>
  "  </p>\n</div>\n"
renderBlock (DropdownBlock teaser blocks) =
  "<details>\n  <summary>" <> escapeHtml teaser <> "</summary>\n" <>
  T.concat (map renderBlock blocks) <>
  "</details>\n"

renderListItem :: ListItem -> Text
renderListItem (ListItem _ text) = 
  "  <li>" <> escapeHtml text <> "</li>\n"

renderInlines :: Text -> Text
renderInlines text = 
  case parseInlines text of
    Left _ -> escapeHtml text
    Right inlines -> T.concat (map renderInline inlines)

renderInline :: Inline -> Text
renderInline (Span text formats) = 
  foldr wrapFormat (escapeHtml text) formats
renderInline (InlineCode code) = 
  "<span class=\"inline-code\">" <> escapeHtml code <> "</span>"
renderInline (InlineMath math) = 
  "<span class=\"inline-math\">$" <> escapeHtml math <> "$</span>"
renderInline (Link linkType url inlines) = 
  let linkText = T.concat (map renderInline inlines)
  in case linkType of
    External -> "<a href=\"" <> url <> "\">" <> linkText <> "</a>"
    Internal -> "<a href=\"" <> url <> "\">" <> linkText <> "</a>"
renderInline (InlineImage alt url) = 
  "<img src=\"" <> url <> "\" alt=\"" <> alt <> "\">"
renderInline (FootnoteRef num) =
  "<sup><a class=\"footnote-ref\" id=\"src" <> T.pack (show num) <> 
  "\" href=\"#fn" <> T.pack (show num) <> 
  "\" data-footnote-id=\"" <> T.pack (show num) <> "\">[" <> 
  T.pack (show num) <> "]</a></sup>"

wrapFormat :: Format -> Text -> Text
wrapFormat Bold text = "<strong>" <> text <> "</strong>"
wrapFormat Italic text = "<em>" <> text <> "</em>"
wrapFormat (Color color) text = "<span class=\"" <> color <> "\">" <> text <> "</span>"

escapeHtml :: Text -> Text
escapeHtml = T.concatMap escapeChar
  where
    escapeChar '<' = "&lt;"
    escapeChar '>' = "&gt;"
    escapeChar '&' = "&amp;"
    escapeChar '"' = "&quot;"
    escapeChar '\'' = "&#39;"
    escapeChar c = T.singleton c