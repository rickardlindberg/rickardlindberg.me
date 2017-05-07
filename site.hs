{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Data.List
import Data.Monoid
import Hakyll
import System.Environment
import System.FilePath
import Network.HTTP.Base

myFeedConfiguration :: FeedConfiguration
myFeedConfiguration = FeedConfiguration
    { feedTitle       = "Rickard's personal homepage: latest posts"
    , feedDescription = "Rickard's personal homepage: latest posts"
    , feedAuthorName  = "Rickard Lindberg"
    , feedAuthorEmail = "ricli85@gmail.com"
    , feedRoot        = "http://rickardlindberg.me"
    }

rules :: Bool -> Rules ()
rules isBuildTargetWebserver = do

    match "templates/*" $ do
        compile templateCompiler

    match "cv-rickard-lindberg.pdf" $ do
        verbatimCopy
    match "static/**" $ do
        verbatimCopy
    match "avatar.png" $ do
        verbatimCopy
    match "writing/**/*.png" $ do
        verbatimCopy
    match "writing/**/*.jpg" $ do
        verbatimCopy

    create ["index.html"] $ do
        route idRoute
        compile $ do
            makeItem ""
                >>= loadAndApplyTemplate "templates/index.html" (recentPostsContext isBuildTargetWebserver)
                >>= loadAndApplyTemplate "templates/default.html" (bodyField "body")
                >>= processUrls isBuildTargetWebserver

    create ["atom.xml"] $ do
        route idRoute
        compile $ do
            let feedContext = postContext isBuildTargetWebserver `mappend` bodyField "description"
            posts <- recentFirst =<< loadAllSnapshots allPosts "postContentOnly"
            renderAtom myFeedConfiguration feedContext posts

    create ["rss.xml"] $ do
        route idRoute
        compile $ do
            let feedContext = postContext isBuildTargetWebserver `mappend` bodyField "description"
            posts <- recentFirst =<< loadAllSnapshots allPosts "postContentOnly"
            renderRss myFeedConfiguration feedContext posts

    match htmlPostPattern $ do
        htmlPost isBuildTargetWebserver

    match "contact/index.markdown" $ do
        page isBuildTargetWebserver

    match "writing/index.markdown" $ do
        pageAsTemplate
            isBuildTargetWebserver
            (createPostsContext isBuildTargetWebserver RecentFirst [("posts", allPosts)])

    match postsWithOwnTitlePattern $ do
        postWithOwnTitle isBuildTargetWebserver

    match postsWithDirectoryNamePattern $ do
        post isBuildTargetWebserver

    match "writing/reflections-on-programming/index.markdown" $ do
        pageAsTemplate
            isBuildTargetWebserver
            (createPostsContext isBuildTargetWebserver Chronological
                [("posts", reflectionsOnProgrammingPattern)])
    match reflectionsOnProgrammingPattern $ do
        routeIndex
        compilePost isBuildTargetWebserver

    match "writing/thought-of-the-day/index.markdown" $ do
        pageAsTemplate
            isBuildTargetWebserver
            (createPostsContext isBuildTargetWebserver Chronological
                [ ("thoughts", thoughtOfTheDay1Pattern)
                , ("thoughts2", thoughtOfTheDay2Pattern)
                ])
    match thoughtOfTheDay1Pattern $ do
        routeUpIndex
        compilePost isBuildTargetWebserver
    match thoughtOfTheDay2Pattern $ do
        routeUpIndex
        compilePost isBuildTargetWebserver

allPosts :: Pattern
allPosts =
         thoughtOfTheDay1Pattern
    .||. thoughtOfTheDay2Pattern
    .||. reflectionsOnProgrammingPattern
    .||. postsWithDirectoryNamePattern
    .||. postsWithOwnTitlePattern
    .||. htmlPostPattern

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
    .||. "writing/problem-in-commit-message/index.markdown"
    .||. "writing/search-and-replace-in-vim/index.markdown"
    .||. "writing/tell-dont-ask-example/index.markdown"
    .||. "writing/bitten-by-python-generators/index.markdown"

postsWithOwnTitlePattern :: Pattern
postsWithOwnTitlePattern =
    "writing/ardour-latency-free-overdubbing/index.rst"

htmlPostPattern :: Pattern
htmlPostPattern =
    "writing/*/index.html"

verbatimCopy :: Rules ()
verbatimCopy = do
    route idRoute
    compile copyFileCompiler

htmlPost :: Bool -> Rules ()
htmlPost isBuildTargetWebserver = do
    route idRoute
    compile $ getResourceBody
        >>= loadAndApplyTemplate "templates/title.html" (baseContext isBuildTargetWebserver)
        >>= processUrls isBuildTargetWebserver
        >>= saveSnapshot "postContentOnly"
        >>= loadAndApplyTemplate "templates/default.html" (baseContext isBuildTargetWebserver)
        >>= processUrls isBuildTargetWebserver

page :: Bool -> Rules ()
page isBuildTargetWebserver = do
    route $ setExtension "html"
    compile $ pandocCompiler
        >>= loadAndApplyTemplate "templates/title.html" (baseContext isBuildTargetWebserver)
        >>= loadAndApplyTemplate "templates/default.html" (baseContext isBuildTargetWebserver)
        >>= processUrls isBuildTargetWebserver

pageAsTemplate :: Bool -> Context String -> Rules ()
pageAsTemplate isBuildTargetWebserver context = do
    route $ setExtension "html"
    compile $ getResourceBody
        >>= applyAsTemplate context
        >>= renderPandoc
        >>= loadAndApplyTemplate "templates/title.html" (baseContext isBuildTargetWebserver)
        >>= loadAndApplyTemplate "templates/default.html" (baseContext isBuildTargetWebserver)
        >>= processUrls isBuildTargetWebserver

post :: Bool -> Rules ()
post isBuildTargetWebserver = do
    route $ setExtension "html"
    compilePost isBuildTargetWebserver

postWithOwnTitle :: Bool -> Rules ()
postWithOwnTitle isBuildTargetWebserver = do
    route $ setExtension "html"
    compile $ pandocCompiler
        >>= processUrls isBuildTargetWebserver
        >>= saveSnapshot "postContentOnly"
        >>= loadAndApplyTemplate "templates/default.html" (baseContext isBuildTargetWebserver)
        >>= processUrls isBuildTargetWebserver

data PostOrder = RecentFirst | Chronological

createPostsContext :: Bool -> PostOrder -> [(String, Pattern)] -> Context String
createPostsContext isBuildTargetWebserver postOrder = foldr
    (\(key, pattern) x -> listField key (postContext isBuildTargetWebserver) (load postOrder pattern) `mappend` x)
    (baseContext isBuildTargetWebserver)
    where
        load RecentFirst pattern = recentFirst =<< loadAll pattern
        load Chronological pattern = chronological =<< loadAll pattern

recentPostsContext :: Bool -> Context String
recentPostsContext isBuildTargetWebserver =
    listField "posts" (postContext isBuildTargetWebserver) (fmap (take 5) . recentFirst =<< loadAll allPosts)
    `mappend`
    (baseContext isBuildTargetWebserver)

compilePost :: Bool -> Rules ()
compilePost isBuildTargetWebserver = compile $ pandocCompiler
    >>= loadAndApplyTemplate "templates/title.html" (postContext isBuildTargetWebserver)
    >>= processUrls isBuildTargetWebserver
    >>= saveSnapshot "postContentOnly"
    >>= loadAndApplyTemplate "templates/default.html" (baseContext isBuildTargetWebserver)
    >>= processUrls isBuildTargetWebserver

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

postContext :: Bool -> Context String
postContext isBuildTargetWebserver = dateField "date" "%e %B %Y" `mappend`
              (baseContext isBuildTargetWebserver)

baseContext :: Bool -> Context String
baseContext isBuildTargetWebserver =
    if isBuildTargetWebserver
        then
            field "url" (fmap (maybe "" (stripIndexHtml . toUrl)) . getRoute . itemIdentifier)
            `mappend`
            defaultContext
        else defaultContext

main :: IO ()
main = hasHakyllBuildTarget "webserver" >>= hakyll . rules

hasHakyllBuildTarget :: String -> IO Bool
hasHakyllBuildTarget target = fmap (elem ("HAKYLL_BUILD_TARGET", target))
                                   getEnvironment

processUrls :: Bool -> Item String -> Compiler (Item String)
processUrls isBuildTargetWebserver x =
    if isBuildTargetWebserver
        then relativizeUrls x >>= deIndexUrls
        else relativizeUrls x

deIndexUrls :: Item String -> Compiler (Item String)
deIndexUrls item = return $ fmap (withUrls stripIndexHtml) item

stripIndexHtml :: String -> String
stripIndexHtml url =
    if "index.html" `isSuffixOf` url && (head url) `elem` "/."
        then take (length url - 10) url
        else url
