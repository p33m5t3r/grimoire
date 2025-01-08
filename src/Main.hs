module Main where
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))

import Compile (AppConfig(..), compileAll, compileOne)


{- usage
compile everything:     grimoire compile
compile one post:       grimoire compile post.md
run web server (TODO):  grimoire run
run dev server (TODO):  grimoire run --dev
-}

defaultConfig :: IO AppConfig
defaultConfig = do
    cwd <- getCurrentDirectory
    -- let baseDir = cwd </> "site"
    let baseDir = cwd </> ".." </> "site"
    
    return AppConfig
        { postsDir          = baseDir </> "posts"
        , compiledDir       = baseDir </> "compiled"
        , postTemplatePath  = baseDir </> "templates/post.html"
        , indexTemplatePath = baseDir </> "templates/index.html"
        , indexHtmlPath     = baseDir </> "compiled/index.html"
        }

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> printUsage
        (cmd:rest) -> case cmd of
            "compile" -> handleCompile rest
            "run"    -> handleRun rest
            _        -> do
                putStrLn $ "Unknown command: " ++ cmd
                printUsage
                exitFailure

handleCompile :: [String] -> IO ()
handleCompile [] = do
    cfg <- defaultConfig
    print cfg
    putStrLn "Compiling all posts..."
    defaultConfig >>= compileAll
    
handleCompile files = do
    config <- defaultConfig
    mapM_  (\f -> putStrLn $ "Compiling post: " ++ f) files
    mapM_ (`compileOne` config) files
    
handleRun :: [String] -> IO ()
handleRun args = do
    let isDev = not (null args) && "--dev" `elem` args
    if isDev
        then putStrLn "dev server coming soon..."  
        else putStrLn "prod server coming soon..."

printUsage :: IO ()
printUsage = do
    putStrLn "Usage:"
    putStrLn "  grimoire compile         # Compile all posts"
    putStrLn "  grimoire compile POST    # Compile single post"
    putStrLn "  grimoire run            # Run web server"
    putStrLn "  grimoire run --dev      # Run development server"

