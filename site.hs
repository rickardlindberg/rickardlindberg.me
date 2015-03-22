{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Data.List
import Data.Monoid
import Hakyll
import System.Environment
import System.FilePath

main :: IO ()
main = hasHakyllBuildTarget "webserver" >>= \shouldDeIndexUrls -> hakyll $ do

    let processUrls x = if shouldDeIndexUrls
                           then relativizeUrls x >>= deIndexUrls
                           else relativizeUrls x

    match "index.markdown" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/title.html" defaultContext
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= processUrls

    match "projects/index.textile" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/title.html" defaultContext
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= processUrls

    let reflectionsOnProgrammingContext = listField "posts" postContext (loadAll "writing/reflections-on-programming/*.textile")
                                          `mappend`
                                          defaultContext

    match "writing/reflections-on-programming/index.markdown" $ do
        route $ setExtension "html"
        compile $ getResourceBody
            >>= applyAsTemplate reflectionsOnProgrammingContext
            >>= return . renderPandoc
            >>= loadAndApplyTemplate "templates/title.html" defaultContext
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= processUrls

    match "writing/reflections-on-programming/*.textile" $ do
        route $ customRoute (\identifier ->
            let filePath = toFilePath identifier
            in  takeDirectory filePath
                </> takeBaseName filePath
                </> "index.html")
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/title.html" postContext
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= processUrls

    match "writing/ardour-latency-free-overdubbing/index.rst" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= processUrls

    match "writing/xmodmap-on-fedora/index.markdown" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/title.html" postContext
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= processUrls

    post processUrls "writing/python-danger-implicit-if/index.markdown"

    let thoughtOfTheDayContext = listField "thoughts" postContext (loadAll "writing/thought-of-the-day/thoughts/*")
                                 `mappend`
                                 listField "thoughts2" postContext (loadAll "writing/thought-of-the-day/thoughts2/*")
                                 `mappend`
                                 defaultContext

    match "writing/thought-of-the-day/index.markdown" $ do
        route $ setExtension "html"
        compile $ getResourceBody
            >>= applyAsTemplate thoughtOfTheDayContext
            >>= return . renderPandoc
            >>= loadAndApplyTemplate "templates/title.html" defaultContext
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= processUrls

    match "writing/thought-of-the-day/thoughts/*.markdown" $ do
        route $ customRoute (\identifier ->
            let filePath = toFilePath identifier
            in  (takeDirectory . takeDirectory) filePath
                </> takeBaseName filePath
                </> "index.html")
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/title.html" postContext
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= processUrls

    match "writing/thought-of-the-day/thoughts2/*.markdown" $ do
        route $ customRoute (\identifier ->
            let filePath = toFilePath identifier
            in  (takeDirectory . takeDirectory) filePath
                </> takeBaseName filePath
                </> "index.html")
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/title.html" postContext
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= processUrls

    verbatimCopy "css/**"
    verbatimCopy "images/**"
    verbatimCopy "writing/**/*.png"
    verbatimCopy "writing/**/*.jpg"

    match "templates/*" $ compile templateCompiler

verbatimCopy :: Pattern -> Rules ()
verbatimCopy pattern = match pattern $ do
    route idRoute
    compile copyFileCompiler

post :: (Item String -> Compiler (Item String)) -> Pattern -> Rules ()
post processUrls pattern = match pattern $ do
    route $ setExtension "html"
    compile $ pandocCompiler
        >>= loadAndApplyTemplate "templates/title.html" postContext
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= processUrls

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

postContext :: Context String
postContext = dateField "date" "%e %B %Y" `mappend`
              defaultContext
