module Compile where
import System.IO (FilePath, readFile)
import System.FilePath ((</>))
import qualified Data.Map as Map
import Control.Monad (forM_, forM, unless)
import Data.Maybe (fromMaybe)
import Control.Exception (try)
import System.Directory (listDirectory
                        ,createDirectoryIfMissing
                        ,doesDirectoryExist
                        )


-- TODO:
-- refactor to data.Text
-- track changes somewhere so we don't have to rebuild everything

{- layout
app.hs
src/    (.hs files)
templates/
    index.html
    post.html
    some_page.html

static/
    images/
        image.png
        image2.png
    style.css
    scripts.js
    fonts/
        firaCode.woff2
posts/
    some_post.md

compiled/
    index.html
    some_post.html
    some_page.html
-}

type PostMetadata = Map.Map String String

testMeta :: PostMetadata
testMeta = Map.fromList [("title", "some post"), ("date", "1/1/1")]

data AppConfig = AppConfig
    { postDir       :: String
    , templateDir   :: String
    , compiledDir   :: String
    }

-- TODO ========
type Context = String
parsePost :: String -> (String, PostMetadata)
parsePost src = ("parsed html", Map.empty)

singletonContext :: String -> String -> Context
singletonContext k v = "context: " ++ v

template :: String -> Context -> String
template t c = t ++ " templated w/ " ++ c
-- ==============

indexContextFrom :: [PostMetadata] -> Context
indexContextFrom _ = "index context"

compileIndex :: AppConfig -> [PostMetadata] -> IO ()
compileIndex cfg metadata =
    readFile templatePath   -- WARNING: lazy, unsafe
    >>= (\t -> writeFile outPath $ template t $ indexContextFrom metadata)
    >> putStrLn ("compiled " ++ outPath)
    where outPath      = compiledDir cfg </> "index.html"
          templatePath = templateDir cfg </> "index.html"


compilePost :: FilePath -> FilePath -> FilePath -> IO PostMetadata
compilePost postPath outPath templatePath = do
    p <- readFile postPath       -- WARNING: lazy, unsafe
    t <- readFile templatePath
    let (innerHtml, meta) = parsePost p
    writeFile outPath $ template t $ singletonContext "post" innerHtml
    putStrLn $ "compiled " ++ postPath ++ " to " ++ outPath ++ " with " ++ templatePath
    return meta

-- TODO: make this actually robust
-- changeExtension "md" "file.html" = file.md"
changeExtension :: String -> String -> String
changeExtension e f = head (words $ rep '.' ' ' f) ++ '.':e
    where rep t n = map (\x -> if x == t then n else x)

compilePosts :: AppConfig -> IO [PostMetadata]
compilePosts cfg = do
    files <- listDirectory (postDir cfg)
    let postPaths = map makePostPaths files
    -- forM_ postPaths print  -- for debugging
    forM postPaths $ \(src, dest, template) -> 
        compilePost src dest template

  where
    postTemplate = "post.html"
    makePostPaths file = 
        ( postDir cfg </> file                                -- source markdown file
        , changeExtension "html" (compiledDir cfg </> file)   -- destination html
        , templateDir cfg </> postTemplate                    -- template file
        )


compile :: IO ()
compile = do
    -- take as param in future
    let cfg = AppConfig { 
          postDir       = "posts"
        , templateDir   = "templates"
        , compiledDir   = "compiled"
        }

    ensureDirectory $ compiledDir cfg

    postData <- compilePosts cfg
    compileIndex cfg postData


ensureDirectory :: FilePath -> IO Bool
ensureDirectory path = do
    existed <- doesDirectoryExist path
    result <- try (createDirectoryIfMissing True path) :: IO (Either IOError ())
    case result of
        Left err -> do
            putStrLn $ "Error creating directory " ++ path ++ ": " ++ show err
            return False
        Right _ -> do
            unless existed $ putStrLn $ "Created directory \"" ++ path ++ "\""
            return True
