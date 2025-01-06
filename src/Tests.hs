module Tests where
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



