module Post where
import qualified Data.Map as Map
import Templating (Template(..), Context(..), render, parseTemplate)
import Markdown (renderMarkdown, parseMarkdown, MarkdownItem)
import Parser (Parser(..), Input(..), newInput, anyCharTill, ParseError)
import Data.Maybe (fromMaybe)
import Yaml (yamlParser, YamlValue(..))
import Control.Monad.Trans.Except
import Control.Monad.IO.Class (liftIO)
import Control.Monad (forM, forM_, unless)
import Control.Exception (try)
import System.Directory (listDirectory
                        ,createDirectoryIfMissing
                        ,doesDirectoryExist
                        )
import System.FilePath ((</>))

-- TODO: compile a post.md file to post.hmtl with a template
--       compile index.html from a template

type PostMetadata = Map.Map String String

data AppConfig = AppConfig
    { postsDir          :: FilePath
    , compiledDir       :: FilePath
    , postTemplatePath  :: FilePath
    , indexTemplatePath :: FilePath
    , indexHtmlPath     :: FilePath
    }

compile :: AppConfig -> IO ()
compile cfg = do
    result <- runExceptT $ do

        -- Read and parse templates
        postTemplateStr <- liftIO $ readFile $ postTemplatePath cfg
        postTemplate <- ExceptT . return . mapLeft show $ parseTemplate postTemplateStr
        
        indexTemplateStr <- liftIO $ readFile $ indexTemplatePath cfg
        indexTemplate <- ExceptT . return . mapLeft show $ parseTemplate indexTemplateStr

        -- make sure compiled directory exists
        liftIO $ ensureDirectory $ compiledDir cfg

        -- read posts
        filenames <- liftIO $ listDirectory $ postsDir cfg
        let posts = map postIOPath filenames

        -- Process all posts
        compiledPosts <- forM posts $ \(inPath, outPath) -> do
            content <- liftIO $ readFile inPath
            (html, meta) <- ExceptT . return $ compilePost (fst postTemplate) content
            liftIO $ writeFile outPath html
            return meta
            
        -- Compile and write index
        indexHtml <- ExceptT . return $ compileIndex (fst indexTemplate) compiledPosts
        liftIO $ writeFile (indexHtmlPath cfg) indexHtml
        
        return $ length compiledPosts

    case result of
        Left err -> putStrLn $ "Compilation failed: " ++ err
        Right n -> putStrLn $ "Successfully compiled " ++ show n ++ " posts"

 where rep t n = map (\x -> if x == t then n else x)
       changeExtension e f = head (words $ rep '.' ' ' f) ++ '.':e
       postIOPath relpath = (postsDir cfg </> relpath
                            ,changeExtension "html" (compiledDir cfg </> relpath))

compileIndex :: Template -> [PostMetadata] -> Either String String
compileIndex t posts = render t $ indexContext posts

indexContext :: [PostMetadata] -> Context
indexContext ps = Object $ Map.singleton "posts" (Array $ map metaToContext ps)
    where metaToContext p = Object $ Map.map String p

compilePost :: Template -> String -> Either String (String, PostMetadata)
compilePost t src = do
    (rawMeta, rest) <- splitPostMeta src
    metadata        <- parseMeta rawMeta
    parsedMd        <- parseMarkdown rest
    html            <- render t $ postContext parsedMd metadata
    return (html, metadata)

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
yamlToMetadata value = Left $ "invalid yaml: " ++ show value

unwrap :: (String, YamlValue) -> Either String (String, String)
unwrap (k, YString v) = Right (k, v)
unwrap (k, v) = Left $ "invalid yaml key: " ++ show v

-- derives the rendering context for a post
postContext :: [MarkdownItem] -> PostMetadata -> Context
postContext doc meta = let contents  = renderMarkdown doc
                           title     = getPostTitle meta
                           footnotes = "todo" in
    Object $ Map.fromList [
        ("contents", String contents),
        ("footnotes", String footnotes),
        ("title", String title)
    ]

-- accessor functions for postMetadata
getPostTitle :: PostMetadata -> String
getPostTitle meta = Data.Maybe.fromMaybe 
                        "untitled" 
                        (Map.lookup "title" meta)


-- misc
mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left x) = Left (f x)
mapLeft _ (Right x) = Right x


test :: Either String (String, PostMetadata, String)
test = do
    (post, meta) <- compilePost myParsedTemplate myPostSrc
    indexTemplate <- case parseTemplate myIndexTemplate of
        Left err -> Left $ show err
        Right (t, i) -> Right t
    index <- compileIndex indexTemplate [meta]
    return (post, meta, index)


myPostSrc :: String
myPostSrc = "title: post title\n\n#this is markdown!\n\n and ur reading it"

myPostTemplate :: String
myPostTemplate = "<html><div> {{contents}} </div></html>"

myParsedTemplate :: Template
myParsedTemplate = case parseTemplate myPostTemplate of
    Left err -> undefined
    Right t -> fst t

myIndexTemplate :: String
myIndexTemplate = "{% for post in posts %} <p> {{post.title}} </p> {% endfor %}"


ensureDirectory :: FilePath -> IO ()
ensureDirectory path = do
    existed <- doesDirectoryExist path
    result <- try (createDirectoryIfMissing True path) :: IO (Either IOError ())
    case result of
        Left err -> putStrLn $ "Error creating directory " ++ path ++ ": " ++ show err
        Right _ -> unless existed $ putStrLn $ "Created directory \"" ++ path ++ "\""


