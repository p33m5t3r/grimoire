module Tests where

import qualified Data.Map as Map

import Markdown
import Parser
import Templating 

allTests :: [IO ()]
allTests = 
    [ runTemplateTests
    , runMarkdownTests
    ]

main :: IO ()
main = sequence_ allTests

-- Templating =========================================================
runTemplateTests :: IO ()
runTemplateTests = do
    putStrLn "\ntemplating:"
    runTestExpect testRender
    runTestExpect testParseTemplate

-- tests rendering a basic Template with a Context
testRender :: TestExpect (Template, Context) (Either String String)
testRender = Test {
    name = "render catchall",
    fn = uncurry renderTemplate,
    x  = (testTemplate , testContext),
    y  = Right "<div><p>post 1</p><p>post 2</p></div>"}

-- tests parsing a raw template string into a Template
testParseTemplate :: TestExpect String (Either ParseError Template)
testParseTemplate = Test {
    name = "parseTemplate catchall",
    fn   = parseTemplate',
    x    = testRawTemplate,
    y    = Right testTemplate
    } 
 where parseTemplate' x' = case parseTemplate x' of
        Left e -> Left e
        Right (t, i) -> Right t

testContext :: Context
testContext = Object $ Map.fromList [
    ("posts", Array [
        Object $ Map.fromList [("contents", String "post 1")],
        Object $ Map.fromList [("contents", String "post 2")]
    ])
  ]

testTemplate :: Template
testTemplate = Block [
    Literal "<div>",
    Loop "posts" "post" (
        Block [
            Literal "<p>",
            Expr ["post", "contents"],
            Literal "</p>"
        ]
    ),
    Literal "</div>"
    ]

testRawTemplate :: String
testRawTemplate = "<div>{% for post in posts %}<p>{{post.contents}}</p>{% endfor %}</div>"


-- Markdown Parser =========================================================
runMarkdownTests :: IO ()
runMarkdownTests = do
    putStrLn "\nMarkdown Parser Tests:"
    mapM_ (runTestExpect . uncurry4 parserTest) atomicMarkdownParserTests
    mapM_ (runTestExpect . uncurry4 parserTest) documentMarkdownParserTests
    where uncurry4 f (a,b,c,d) = f a b c d

atomicMarkdownParserTests :: [(String, Parser MarkdownItem, String, Either ParseError MarkdownItem)]
atomicMarkdownParserTests = 
    [ ("plaintext basic", plaintext, "hello", Right $ Plaintext "hello" None "")
    , ("bold", boldText, "*bold*", Right $ Plaintext "bold" Bold "")
    , ("bold escape", boldText, "*bold\\*text*", Right $ Plaintext "bold*text" Bold "")
    , ("italic", italicText, "_italic_", Right $ Plaintext "italic" Italic "")
    , ("inline code", inlineCode, "```x = 1```", Right $ Code "x = 1" "" False)
    , ("display code", displayCode, "\n```\nx = 1\n```", Right $ Code "x = 1" "" True)
    , ("inline math", inlineMath, "$x + y$", Right $ Math "x + y" False)
    , ("display math", displayMath, "$$x + y$$", Right $ Math "x + y" True)
    , ("level 1 header", header, "# Title", Right $ Header "Title" 1)
    , ("level 2 header", subheader, "## Subtitle", Right $ Header "Subtitle" 2)
    , ("basic link", link, "[text](url)", Right $ Link "text" "url")
    , ("basic image", image, "![alt](url)", Right $ Image "alt" "url")
    , ("basic quote", quote, "> quoted text", Right $ Quote "quoted text")
    , ("footnote reference", footref, "[^1]", Right $ FootRef 1)
    , ("footnote definition", footdef, "[^1]: footnote text:[1]"
      , Right $ FootDef 1 [Plaintext "footnote text" None ""]
      )
    , ("basic html", rawHtml, "<div>", Right $ RawHtml "div")
    , ("basic dropdown", dropdown, ".[Click me\nContent.]"
      , Right $ Dropdown "Click me" [Plaintext "Content" None ""]
      )
    , ("basic paragraph", paragraph, "normal *bold* normal" 
      , Right $ Paragraph
        [Plaintext "normal" None "", Plaintext "bold" Bold "", Plaintext "normal" None ""]
      )
    ]

documentMarkdownParserTests :: [(String, Parser [MarkdownItem], String, Either a [MarkdownItem])]
documentMarkdownParserTests = 
    [ ("basic document", document, "\n\npara 1\n\npara 2\n\n"
      , Right [
            Paragraph [Plaintext "para 1" None ""],
            Paragraph [Plaintext "para 2" None ""]
        ]
      )
    ]

-- Internals =======================================================

-- Test case constructor
parserTest :: Show a => String -> Parser a -> 
            String -> Either ParseError a 
            -> TestExpect String (Either ParseError a)
parserTest name p = Test name (parse p) 

-- Parser runner
parse :: Parser a -> String -> Either ParseError a
parse p = fmap fst . runP p . newInput

-- Test runner
data TestExpect a b = Test 
    { name :: String
    , fn   :: a -> b
    , x    :: a
    , y    :: b
    }

runTestExpect :: (Eq b, Show b) => TestExpect a b -> IO ()
runTestExpect (Test name fn input expected) = do
    let actual = fn input
        passed = expected == actual
        status = if passed 
                 then "OK" 
                 else "FAIL\nexpected:\n" ++ show expected ++ "\nactual:\n" ++ show actual
    putStrLn $ "Testing " ++ name ++ "..." ++ status


