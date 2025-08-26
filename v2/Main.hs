{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (forM_, unless, filterM)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory
import System.FilePath
import System.Exit (exitFailure)

import Compiler
import CompileError (CompileError, displayError)
import PostMetadata
import Template (Template, parseTemplate, renderTemplate, ToContext(..))

-- Configuration for compilation
data AppConfig = AppConfig
  { postsDir :: FilePath
  , outputDir :: FilePath
  , staticDir :: FilePath  -- for images
  , templatesDir :: FilePath
  } deriving (Show, Eq)

defaultConfig :: AppConfig  
defaultConfig = AppConfig
  { postsDir = "posts"
  , outputDir = "www"
  , staticDir = "www/static/images"
  , templatesDir = "templates"
  }

main :: IO ()
main = do
  putStrLn "Grimoire v2 - Compiling posts..."
  compileAll defaultConfig

compileAll :: AppConfig -> IO ()
compileAll config = do
  -- Create output directories
  createDirectoryIfMissing True (outputDir config)
  createDirectoryIfMissing True (staticDir config)
  
  -- Load post template  
  postTemplate <- loadTemplate config "post.html"
  
  -- Discover post directories
  postDirs <- discoverPostDirs (postsDir config)
  
  if null postDirs
    then do
      putStrLn $ "No post directories found in " ++ postsDir config
      exitFailure
    else do
      putStrLn $ "Found " ++ show (length postDirs) ++ " posts to compile"
      
      -- Compile each post
      results <- mapM (compilePostDir config postTemplate) postDirs
      let failures = [err | Left err <- results]
      
      if null failures
        then putStrLn "All posts compiled successfully!"
        else do
          putStrLn $ "Compilation failed for " ++ show (length failures) ++ " posts:"
          forM_ failures (TIO.putStrLn . displayError)
          exitFailure

-- Discover all post directories (containing post.mdx)
discoverPostDirs :: FilePath -> IO [FilePath]
discoverPostDirs postsRoot = do
  exists <- doesDirectoryExist postsRoot
  unless exists $ do
    putStrLn $ "Posts directory does not exist: " ++ postsRoot
    exitFailure
  
  entries <- listDirectory postsRoot
  postDirs <- filterM hasPostFile entries
  return postDirs
  where
    hasPostFile dir = do
      let postPath = postsRoot </> dir </> "post.mdx" 
      doesFileExist postPath

-- Compile a single post directory  
compilePostDir :: AppConfig -> Template -> FilePath -> IO (Either CompileError ())
compilePostDir config template postDir = do
  let postPath = postsDir config </> postDir </> "post.mdx"
      outputPath = outputDir config </> postDir <.> "html"
      slug = T.pack postDir
  
  putStrLn $ "Compiling: " ++ postPath ++ " -> " ++ outputPath
  
  -- Read post file
  content <- TIO.readFile postPath
  
  -- Parse and compile (pure functions)
  case parsePost slug content of
    Left err -> return $ Left err
    Right (metadata, document) -> do
      let articleHtml = compilePost metadata document
          postCtx = PostContext metadata articleHtml
      
      -- Render with template
      case renderTemplate template (toContext postCtx) of
        Left err -> return $ Left err
        Right fullHtml -> do
          TIO.writeFile outputPath fullHtml
          return $ Right ()

-- Load and parse a template file
loadTemplate :: AppConfig -> FilePath -> IO Template
loadTemplate config templateName = do
  let templatePath = templatesDir config </> templateName
  templateContent <- TIO.readFile templatePath
  case parseTemplate templateContent of
    Left err -> do
      putStrLn $ "Failed to parse template " ++ templateName ++ ": " ++ T.unpack (displayError err)
      exitFailure  
    Right template -> return template