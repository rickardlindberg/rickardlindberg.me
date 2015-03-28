{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Data.List
import Data.Monoid
import Hakyll
import System.Environment
import System.FilePath

rules :: (Item String -> Compiler (Item String)) -> Rules ()
rules processUrls = do

    match "templates/*" $ do
        compile templateCompiler

    match "css/**" $ do
        verbatimCopy
    match "images/**" $ do
        verbatimCopy
    match "writing/**/*.png" $ do
        verbatimCopy
    match "writing/**/*.jpg" $ do
        verbatimCopy

    create ["index.html"] $ do
        route idRoute
        compile $ do
            let ctx = (createPostsContext RecentFirst [("posts", allPosts)])
            makeItem ""
                >>= loadAndApplyTemplate "templates/index.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= processUrls

    match "projects/index.textile" $ do
        page processUrls

    match postsWithOwnTitlePattern $ do
        postWithOwnTitle processUrls

    match postsWithDirectoryNamePattern $ do
        post processUrls

    match "writing/reflections-on-programming/index.markdown" $ do
        pageAsTemplate
            (createPostsContext Chronological
                [("posts", reflectionsOnProgrammingPattern)])
            processUrls
    match reflectionsOnProgrammingPattern $ do
        routeIndex
        compilePost processUrls

    match "writing/thought-of-the-day/index.markdown" $ do
        pageAsTemplate
            (createPostsContext Chronological
                [ ("thoughts", thoughtOfTheDay1Pattern)
                , ("thoughts2", thoughtOfTheDay2Pattern)
                ])
            processUrls
    match thoughtOfTheDay1Pattern $ do
        routeUpIndex
        compilePost processUrls
    match thoughtOfTheDay2Pattern $ do
        routeUpIndex
        compilePost processUrls

allPosts :: Pattern
allPosts =
         thoughtOfTheDay1Pattern
    .||. thoughtOfTheDay2Pattern
    .||. reflectionsOnProgrammingPattern
    .||. postsWithDirectoryNamePattern
    .||. postsWithOwnTitlePattern

thoughtOfTheDay1Pattern :: Pattern
thoughtOfTheDay1Pattern =
    "writing/thought-of-the-day/thoughts/*.markdown"

thoughtOfTheDay2Pattern :: Pattern
thoughtOfTheDay2Pattern =
    "writing/thought-of-the-day/thoughts2/*.markdown"

reflectionsOnProgrammingPattern :: Pattern
reflectionsOnProgrammingPattern =
    "writing/reflections-on-programming/*.textile"

postsWithDirectoryNamePattern :: Pattern
postsWithDirectoryNamePattern =
         "writing/xmodmap-on-fedora/index.markdown"
    .||. "writing/python-danger-implicit-if/index.markdown"
    .||. "writing/search-and-replace-in-vim/index.markdown"

postsWithOwnTitlePattern :: Pattern
postsWithOwnTitlePattern =
    "writing/ardour-latency-free-overdubbing/index.rst"

verbatimCopy :: Rules ()
verbatimCopy = do
    route idRoute
    compile copyFileCompiler

page :: (Item String -> Compiler (Item String)) -> Rules ()
page processUrls = do
    route $ setExtension "html"
    compile $ pandocCompiler
        >>= loadAndApplyTemplate "templates/title.html" defaultContext
        >>= loadAndApplyTemplate "templates/footer.html" defaultContext
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= processUrls

pageAsTemplate :: Context String -> (Item String -> Compiler (Item String)) -> Rules ()
pageAsTemplate context processUrls = do
    route $ setExtension "html"
    compile $ getResourceBody
        >>= applyAsTemplate context
        >>= return . renderPandoc
        >>= loadAndApplyTemplate "templates/title.html" defaultContext
        >>= loadAndApplyTemplate "templates/footer.html" defaultContext
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= processUrls

post :: (Item String -> Compiler (Item String)) -> Rules ()
post processUrls = do
    route $ setExtension "html"
    compilePost processUrls

postWithOwnTitle :: (Item String -> Compiler (Item String)) -> Rules ()
postWithOwnTitle processUrls = do
    route $ setExtension "html"
    compile $ pandocCompiler
        >>= loadAndApplyTemplate "templates/footer.html" defaultContext
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= processUrls

data PostOrder = RecentFirst | Chronological

createPostsContext :: PostOrder -> [(String, Pattern)] -> Context String
createPostsContext postOrder = foldr
    (\(key, pattern) x -> listField key postContext (load postOrder pattern) `mappend` x)
    defaultContext
    where
        load RecentFirst pattern = recentFirst =<< loadAll pattern
        load Chronological pattern = chronological =<< loadAll pattern

compilePost :: (Item String -> Compiler (Item String)) -> Rules ()
compilePost processUrls = compile $ pandocCompiler
    >>= loadAndApplyTemplate "templates/title.html" postContext
    >>= loadAndApplyTemplate "templates/footer.html" defaultContext
    >>= loadAndApplyTemplate "templates/default.html" defaultContext
    >>= processUrls

routeUpIndex :: Rules ()
routeUpIndex = route $ customRoute (\identifier ->
    let filePath = toFilePath identifier
    in  (takeDirectory . takeDirectory) filePath
        </> takeBaseName filePath
        </> "index.html")

routeIndex :: Rules ()
routeIndex = route $ customRoute (\identifier ->
    let filePath = toFilePath identifier
    in  takeDirectory filePath
        </> takeBaseName filePath
        </> "index.html")

postContext :: Context String
postContext = dateField "date" "%e %B %Y" `mappend`
              defaultContext

main :: IO ()
main = hasHakyllBuildTarget "webserver" >>= \shouldDeIndexUrls -> hakyll $ do
    rules $ \x -> if shouldDeIndexUrls
       then relativizeUrls x >>= deIndexUrls
       else relativizeUrls x

hasHakyllBuildTarget :: String -> IO Bool
hasHakyllBuildTarget target = fmap (elem ("HAKYLL_BUILD_TARGET", target))
                                   getEnvironment

deIndexUrls :: Item String -> Compiler (Item String)
deIndexUrls item = return $ fmap (withUrls stripIndexHtml) item

stripIndexHtml :: String -> String
stripIndexHtml url =
    if "index.html" `isSuffixOf` url && (head url) `elem` "/."
        then take (length url - 10) url
        else url
