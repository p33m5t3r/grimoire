module Compile where
import System.IO (FilePath, readFile)
import System.FilePath ((</>))
import qualified Data.Map as Map
import Control.Monad (forM_, forM)
import Data.Maybe (fromMaybe)
import Control.Exception (try)
import System.Directory (listDirectory
                        ,createDirectoryIfMissing
                        )


-- TODO: build the compiler
-- refactor to data.Text

{-
rn it should literally just rebuild everything
eventually we can just track in a file what
... does and doesn't need rebuilding
we can read that file in the main compile fn
so it's type signature wouldn't even change


-}

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


{- build process
1. compile posts, get metadata, urls
2. compile index.html
3. deal with static later
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


compileIndex :: AppConfig -> [PostMetadata] -> IO ()
compileIndex _ _ = return ()
-- ==============

compilePost :: FilePath -> FilePath -> FilePath -> IO PostMetadata
compilePost postPath outPath templatePath = do
    p <- readFile postPath       -- WARNING: lazy
    t <- readFile templatePath
    let (innerHtml, meta) = parsePost p
    -- writeFile outPath $ template t $ singletonContext "post" innerHtml
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
    forM_ postPaths print  -- for debugging
    -- forM postPaths $ \(src, dest, template) -> 
    --    compilePost src dest template

    return [Map.empty]
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

    forM_ postData print


ensureDirectory :: FilePath -> IO Bool
ensureDirectory path = do
    result <- try (createDirectoryIfMissing True path) :: IO (Either IOError ())
    case result of
        Left err -> do
            putStrLn $ "Error creating directory " ++ path ++ ": " ++ show err
            return False
        Right _ -> do
            putStrLn $ "Directory " ++ path ++ " created/verified successfully"
            return True



