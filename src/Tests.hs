module Tests where
import Markdown
import Parser
import Templating (template, render, parseTemplate, Context(..), Template(..))
import qualified Data.Map as Map

runTests :: IO ()
runTests = do
    runTestExpect testRender
    runTestExpect testTemplateFn
    runTestExpect testParseTemplate

runTestExpect :: (Eq b, Show b) => TestExpect a b -> IO ()
runTestExpect t = do
    let expected = y t
    let actual   = fn t (x t)
    let passed   = expected == actual
    let errMsg   = "\nexpected:\n" ++ show expected ++ "\n" ++ "actual:\n" ++ show actual
    let status   = if passed then "OK" else "FAIL" ++ errMsg
    let runMsg   = "testing " ++ name t ++ "..." ++ status
    putStrLn runMsg

data TestExpect a b = Test {name::String, fn::a -> b, x::a, y::b}

-- ==================== Templating.hs ====================
-- tests rendering a basic Template with a Context
testRender :: TestExpect (Template, Context) (Either String String)
testRender = Test {
    name = "render catchall",
    fn = uncurry render,
    x  = (testTemplate , testContext),
    y  = Right "<div><p>post 1</p><p>post 2</p></div>"}

-- tests parsing and rendering a basic template string with a context
testTemplateFn :: TestExpect (String, Context) (Either String String)
testTemplateFn = Test {
    name = "template catchall",
    fn   = uncurry template,
    x    = (testRawTemplate, testContext),
    y    = Right "<div><p>post 1</p><p>post 2</p></div>" }

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


-- Helper to create parser test case
parserTest :: Show a => String -> Parser a -> String -> Either ParseError a -> TestExpect String (Either ParseError a)
parserTest name p input expected = Test {
    name = name,
    fn = parse p,
    x = input,
    y = expected
}

-- Helper to extract just the value from a successful parse
parse :: Parser a -> String -> Either ParseError a
parse p input = case runP p (newInput input) of
    Left err -> Left err
    Right (a, _) -> Right a

-- Test cases for basic text formatting
testPlaintext :: TestExpect String (Either ParseError MarkdownItem)
testPlaintext = parserTest 
    "plaintext basic" 
    plaintext 
    "hello" 
    (Right $ Plaintext "hello" None "")

testBold :: TestExpect String (Either ParseError MarkdownItem)
testBold = parserTest 
    "bold basic" 
    boldText 
    "*bold*" 
    (Right $ Plaintext "bold" Bold "")

testBoldEscape :: TestExpect String (Either ParseError MarkdownItem)
testBoldEscape = parserTest 
    "bold with escape" 
    boldText 
    "*bold\\*text*" 
    (Right $ Plaintext "bold*text" Bold "")

testItalic :: TestExpect String (Either ParseError MarkdownItem)
testItalic = parserTest 
    "italic basic" 
    italicText 
    "_italic_" 
    (Right $ Plaintext "italic" Italic "")

-- Test cases for code blocks
testInlineCode :: TestExpect String (Either ParseError MarkdownItem)
testInlineCode = parserTest 
    "inline code" 
    inlineCode 
    "```x = 1```" 
    (Right $ Code "x = 1" "" False)

testDisplayCode :: TestExpect String (Either ParseError MarkdownItem)
testDisplayCode = parserTest 
    "display code" 
    displayCode 
    "\n```\nx = 1\n```" 
    (Right $ Code "x = 1" "" True)

-- Test cases for math
testInlineMath :: TestExpect String (Either ParseError MarkdownItem)
testInlineMath = parserTest 
    "inline math" 
    inlineMath 
    "$x + y$" 
    (Right $ Math "x + y" False)

testDisplayMath :: TestExpect String (Either ParseError MarkdownItem)
testDisplayMath = parserTest 
    "display math" 
    displayMath 
    "$$x + y$$" 
    (Right $ Math "x + y" True)

-- Test cases for headers
testHeader :: TestExpect String (Either ParseError MarkdownItem)
testHeader = parserTest 
    "level 1 header" 
    header 
    "# Title" 
    (Right $ Header "Title" 1)

testSubheader :: TestExpect String (Either ParseError MarkdownItem)
testSubheader = parserTest 
    "level 2 header" 
    subheader 
    "## Subtitle" 
    (Right $ Header "Subtitle" 2)

-- Test cases for links and images
testLink :: TestExpect String (Either ParseError MarkdownItem)
testLink = parserTest 
    "basic link" 
    link 
    "[text](url)" 
    (Right $ Link "text" "url")

testImage :: TestExpect String (Either ParseError MarkdownItem)
testImage = parserTest 
    "basic image" 
    image 
    "![alt](url)" 
    (Right $ Image "alt" "url")

-- Test cases for quotes
testQuote :: TestExpect String (Either ParseError MarkdownItem)
testQuote = parserTest 
    "basic quote" 
    quote 
    "> quoted text" 
    (Right $ Quote "quoted text")

-- Test cases for footnotes
testFootref :: TestExpect String (Either ParseError MarkdownItem)
testFootref = parserTest 
    "footnote reference" 
    footref 
    "[^1]" 
    (Right $ FootRef 1)

testFootdef :: TestExpect String (Either ParseError MarkdownItem)
testFootdef = parserTest 
    "footnote definition" 
    footdef 
    "[^1]: footnote text:[1]" 
    (Right $ FootDef 1 [Plaintext "footnote text" None ""])

-- Test cases for HTML
testHtml :: TestExpect String (Either ParseError MarkdownItem)
testHtml = parserTest 
    "basic html" 
    rawHtml 
    "<div>" 
    (Right $ RawHtml "div")

-- Test cases for dropdowns
testDropdown :: TestExpect String (Either ParseError MarkdownItem)
testDropdown = parserTest 
    "basic dropdown" 
    dropdown 
    ".[Click me\nContent.]" 
    (Right $ Dropdown "Click me" [Plaintext "Content" None ""])

-- Test cases for paragraphs
testParagraph :: TestExpect String (Either ParseError MarkdownItem)
testParagraph = parserTest 
    "basic paragraph" 
    paragraph 
    "normal *bold* normal" 
    (Right $ Paragraph [
        Plaintext "normal" None "",
        Plaintext "bold" Bold "",
        Plaintext "normal" None ""
    ])

-- Test cases for full documents
testDocument :: TestExpect String (Either ParseError [MarkdownItem])
testDocument = parserTest 
    "basic document" 
    document 
    "\n\npara 1\n\npara 2\n\n" 
    (Right [
        Paragraph [Plaintext "para 1" None ""],
        Paragraph [Plaintext "para 2" None ""]
    ])

-- Run all tests
runMarkdownTests :: IO ()
runMarkdownTests = do
    putStrLn "Running Markdown Parser Tests\n"
    mapM_ runTestExpect
        [ testPlaintext
        , testBold
        , testBoldEscape
        , testItalic
        , testInlineCode
        , testDisplayCode
        , testInlineMath
        , testDisplayMath
        , testHeader
        , testSubheader
        , testLink
        , testImage
        , testQuote
        , testFootref
        , testFootdef
        , testHtml
        , testDropdown
        , testParagraph
        ]
    runTestExpect testDocument

