module Compile 
    ( AppConfig(..)
    , compileAll
    , compileOne
    ) where

import Control.Exception (try)
import Control.Monad (forM, unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import System.FilePath (replaceExtension, (</>))
import System.Directory
    ( createDirectoryIfMissing
    , doesDirectoryExist
    , listDirectory
    )

import Markdown   (MarkdownItem, parseMarkdown, renderMarkdown)
import Parser     (Input(..), Parser(..), anyCharTill, newInput)
import Templating (Context(..), Template(..), parseTemplate, renderTemplate)
import Yaml       (YamlValue(..), yamlParser)


type PostMetadata = Map.Map String String
data AppConfig = AppConfig
    { postsDir          :: FilePath
    , compiledDir       :: FilePath
    , postTemplatePath  :: FilePath
    , indexTemplatePath :: FilePath
    , indexHtmlPath     :: FilePath
    } deriving Show

-- IO ==================================================================
compileAll :: AppConfig -> IO ()
compileAll cfg = compileExcept $ do
    -- Read and parse templates
    postTemplate <- loadTemplate $ postTemplatePath cfg
    indexTemplate <- loadTemplate $ indexTemplatePath cfg

    -- make sure compiled directory exists
    liftIO $ ensureDirectory $ compiledDir cfg

    -- read posts
    filenames <- liftIO $ listDirectory $ postsDir cfg
    let posts = map (postIOPath cfg) filenames

    -- Process all posts
    compiledPosts <- forM posts $ \(inPath, outPath) -> do
        liftIO $ putStrLn $ "compiling '" ++ inPath ++ "' to '" ++ outPath ++ "'"
        content <- liftIO $ readFile inPath
        (html, meta) <- ExceptT . return $ compilePost postTemplate content
        liftIO $ writeFile outPath html
        return meta
        
    -- Compile and write index
    indexHtml <- ExceptT . return $ compileIndex indexTemplate compiledPosts
    liftIO $ writeFile (indexHtmlPath cfg) indexHtml

    return $ "compiled " ++ show (length compiledPosts) ++ " posts"

compileOne :: FilePath -> AppConfig -> IO()
compileOne filename cfg = compileExcept $ do
    postTemplate <- loadTemplate $ postTemplatePath cfg
    liftIO $ ensureDirectory $ compiledDir cfg

    let (inPath, outPath) = postIOPath cfg filename
    liftIO $ putStrLn $ "compiling '" ++ inPath ++ "' to '" ++ outPath ++ "'"
    content <- liftIO $ readFile inPath 
    (html, _) <- ExceptT . return $ compilePost postTemplate content
    
    liftIO $ writeFile outPath html
    return $ "compiled " ++ filename

-- Configurable ==========================================================
compilePost :: Template -> String -> Either String (String, PostMetadata)
compilePost t src = do
    (rawMeta, rest) <- splitPostMeta src
    metadata        <- parseMeta rawMeta
    parsedMd        <- parseMarkdown rest
    html            <- renderTemplate t $ postContext parsedMd metadata
    return (html, metadata)

compileIndex :: Template -> [PostMetadata] -> Either String String
compileIndex t posts = renderTemplate t $ indexContext posts

-- derives the context needed to render the index template
indexContext :: [PostMetadata] -> Context
indexContext ps = Object $ Map.singleton "posts" (Array $ map metaToContext ps)
    where metaToContext p = Object $ Map.map String p

-- derives the context needed to render the post template
postContext :: [MarkdownItem] -> PostMetadata -> Context
postContext doc meta = let contents  = renderMarkdown doc
                           title     = getPostTitle meta
                           footnotes = "todo" in
    Object $ Map.fromList [
        ("contents", String contents),
        ("footnotes", String footnotes),
        ("title", String title)
    ]
    where getPostTitle meta = Data.Maybe.fromMaybe 
                              "untitled" 
                              (Map.lookup "title" meta)

-- Utilities =============================================================
loadTemplate :: FilePath -> ExceptT String IO Template
loadTemplate path = do
    template <- liftIO $ readFile path
    ExceptT . return . mapLeft show . fmap fst $ parseTemplate template

-- "post.md" -> "(posts/post.md, compiled/post.html)"       (fixes relpaths)
postIOPath :: AppConfig -> FilePath -> (FilePath, FilePath)
postIOPath cfg relpath = ( postsDir cfg </> relpath
                         , replaceExtension (compiledDir cfg </> relpath) ".html")

-- splits a post.md into the raw yaml header and the raw md contents
splitPostMeta :: String -> Either String (String, Input)
splitPostMeta p = case runP (anyCharTill "\n\n") $ newInput p of
    Left err -> Left $ show err
    Right (rawYaml, rest) -> Right (rawYaml, rest)

-- parses raw yaml metadata into PostMetadata
parseMeta :: String -> Either String PostMetadata
parseMeta src = case runP yamlParser $ newInput src of
    Left err -> Left $ show err
    Right (y, _) -> case yamlToMetadata y of
        Left err -> Left $ show err
        Right y' -> Right y'

-- converts yaml object to PostMetadata
yamlToMetadata :: YamlValue -> Either String PostMetadata
yamlToMetadata (YObject ks) = case mapM unwrap ks of
    Left err  -> Left err
    Right ks' -> Right $ Map.fromList ks'
  where unwrap (k, YString v) = Right (k, v)
        unwrap (k, v) = Left $ "invalid yaml value: " ++ show v ++ "from: " ++ k
yamlToMetadata value = Left $ "invalid yaml: " ++ show value

compileExcept :: ExceptT String IO String -> IO ()
compileExcept action = do
    result <- runExceptT action
    case result of
        Left err -> putStrLn $ "compilation failed: " ++ err
        Right msg -> putStrLn $ "compilation success: " ++ msg

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left x) = Left (f x)
mapLeft _ (Right x) = Right x

ensureDirectory :: FilePath -> IO ()
ensureDirectory path = do
    existed <- doesDirectoryExist path
    result <- try (createDirectoryIfMissing True path) :: IO (Either IOError ())
    case result of
        Left err -> putStrLn $ "Error creating directory " ++ path ++ ": " ++ show err
        Right _ -> unless existed $ putStrLn $ "Created directory \"" ++ path ++ "\""


