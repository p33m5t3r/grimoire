module Main where
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import System.Directory (getDirectoryContents
                        ,createDirectoryIfMissing
                        )
import System.IO (FilePath)
import Data.Time.Clock (UTCTime)
import qualified Data.Set as Set

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad (forever)


{- usage
grimoire compile
grimoire serve
grimoire serve --dev
-}

type Path = [String]
type Cache = Map.Map String String
type HttpResponse = String

route :: Path -> (Cache -> HttpResponse)
route [] = serveCachedById "index"
route ("posts":rest) = case rest of
    [id, extra] -> const "404 invalid post"
    [id]        -> serveCachedById id
    _ -> const "404 invalid post"
route _ = const "404"

serveCachedById :: String -> Cache -> HttpResponse
serveCachedById s c = fromMaybe "404" (Map.lookup s c)

myAppCache :: Map.Map String String
myAppCache = Map.fromList [
    ("index", "this is index.html"),
    ("post1", "this is post1.html")]

buildCache :: IO Cache
buildCache = do
    -- should also just call compile
    -- we might not even need this
    return myAppCache

parseUrl :: String -> Path
parseUrl u = words $ map (\c -> if c == '/' then ' ' else c) u

untilTerminated :: MVar () -> IO () -> IO ()
untilTerminated tv action = forever $ do
    terminated <- tryReadMVar tv
    case terminated of
        Just _ -> return ()
        Nothing -> action

serveRequests :: MVar Cache -> MVar () -> IO ()
serveRequests cacheVar tv = untilTerminated tv $ do
    print "serving requests"
    input <- getLine
    if input == ":q" 
        then do
            print "request server exiting..."
            putMVar tv ()
            return ()
        else do
            cache <- readMVar cacheVar
            print $ route (parseUrl input) cache


-- modifyMVar_ cacheVar $ \cache -> do
--     let newCache = Map.insert "new_key" "new_value" cache
--     return newCache
--
update :: MVar Cache -> MVar () -> IO ()
update cacheVar tv = untilTerminated tv $ do
    print "cache updating..."

    -- this should just call `compile`
    -- compile should be smart enough to only touch what's necessary
    -- it should return a list of files to re-read into memory

    threadDelay 1000000

main :: IO ()
main = do
    initialCache <- buildCache
    cacheVar <- newMVar initialCache
    termVar <- newEmptyMVar
    
    -- Start server thread
    serverThread <- forkIO $ serveRequests cacheVar termVar
    
    -- Start update thread
    updateThread <- forkIO $ update cacheVar termVar
    
    -- Wait for kill signal
    takeMVar termVar  -- blocks 
    
    putStrLn "shutting down..."
    threadDelay 1_000_000
    putStrLn "shutdown complete"



test :: IO ()
test = do
    cache <- buildCache
    let urls = ["/"
            ,"posts/post1"
            ,"404"
            ,"posts/fakepost"
            ,"posts/post1/wrong"]
    try urls cache
  where try [] _ = return ()
        try (x:xs) c = do
            let input  = x
            let output = route (parseUrl x) c
            putStrLn $ input ++ " -> " ++ output ++ "\n"
            try xs c



