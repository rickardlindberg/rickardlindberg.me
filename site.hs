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

    match ("index.markdown" .||.
           "projects/index.textile") $ do
        page processUrls

    match "writing/ardour-latency-free-overdubbing/index.rst" $ do
        postWithOwnTitle processUrls

    match "writing/xmodmap-on-fedora/index.markdown" $ do
        post processUrls

    match "writing/python-danger-implicit-if/index.markdown" $ do
        post processUrls

    match "writing/search-and-replace-in-vim/index.markdown" $ do
        post processUrls

    match "writing/reflections-on-programming/index.markdown" $ do
        pageAsTemplate
            (createPostsContext "writing/reflections-on-programming/*.textile")
            processUrls
    match "writing/reflections-on-programming/*.textile" $ do
        routeIndex
        compilePost processUrls

    match "writing/thought-of-the-day/index.markdown" $ do
        pageAsTemplate
            (listField "thoughts" postContext (loadAll "writing/thought-of-the-day/thoughts/*")
             `mappend`
             listField "thoughts2" postContext (loadAll "writing/thought-of-the-day/thoughts2/*")
             `mappend`
             defaultContext)
            processUrls
    match "writing/thought-of-the-day/thoughts/*.markdown" $ do
        routeUpIndex
        compilePost processUrls
    match "writing/thought-of-the-day/thoughts2/*.markdown" $ do
        routeUpIndex
        compilePost processUrls

verbatimCopy :: Rules ()
verbatimCopy = do
    route idRoute
    compile copyFileCompiler

page :: (Item String -> Compiler (Item String)) -> Rules ()
page processUrls = do
    route $ setExtension "html"
    compile $ pandocCompiler
        >>= loadAndApplyTemplate "templates/title.html" defaultContext
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= processUrls

pageAsTemplate :: Context String -> (Item String -> Compiler (Item String)) -> Rules ()
pageAsTemplate context processUrls = do
    route $ setExtension "html"
    compile $ getResourceBody
        >>= applyAsTemplate context
        >>= return . renderPandoc
        >>= loadAndApplyTemplate "templates/title.html" defaultContext
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
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= processUrls

createPostsContext :: Pattern -> Context String
createPostsContext pattern =
    listField "posts" postContext (loadAll pattern)
    `mappend`
    defaultContext

compilePost :: (Item String -> Compiler (Item String)) -> Rules ()
compilePost processUrls = compile $ pandocCompiler
    >>= loadAndApplyTemplate "templates/title.html" postContext
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
