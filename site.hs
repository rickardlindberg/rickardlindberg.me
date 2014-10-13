{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Data.List
import Data.Monoid
import Hakyll
import System.Environment
import System.FilePath

main :: IO ()
main = hasHakyllBuildTarget "webserver" >>= \shouldDeIndexUrls -> hakyll $ do

    let postContext = dateField "date" "%B %e, %Y" `mappend` defaultContext

    let processUrls x = if shouldDeIndexUrls
                           then relativizeUrls x >>= deIndexUrls
                           else relativizeUrls x

    match "index.markdown" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/title.html" defaultContext
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= processUrls

    match "writing/ardour-latency-free-overdubbing/index.rst" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/flattr_howto.html" defaultContext
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= processUrls

    match "writing/xmodmap-on-fedora/index.markdown" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/flattr_howto.html" defaultContext
            >>= loadAndApplyTemplate "templates/title.html" defaultContext
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= processUrls

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
            >>= loadAndApplyTemplate "templates/title.html" defaultContext
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= processUrls

    match "writing/thought-of-the-day/thoughts2/*.markdown" $ do
        route $ customRoute (\identifier ->
            let filePath = toFilePath identifier
            in  (takeDirectory . takeDirectory) filePath
                </> takeBaseName filePath
                </> "index.html")
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/title.html" defaultContext
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= processUrls

    idRouteCopy "css/**"
    idRouteCopy "images/**"
    idRouteCopy "writing/**/*.png"
    idRouteCopy "writing/**/*.jpg"
    match "templates/*" $ compile templateCompiler

idRouteCopy pattern = match pattern $ do
    route $ idRoute
    compile $ copyFileCompiler

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
