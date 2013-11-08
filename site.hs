{-# LANGUAGE OverloadedStrings #-}

import Data.List
import System.FilePath

import Hakyll

exportToWebserver = True

main :: IO ()
main = hakyll $ do

    match "index.markdown" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/title.html" defaultContext
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls
            >>= deIndexUrls

    match "writing/ardour-latency-free-overdubbing/index.rst" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls
            >>= deIndexUrls

    match "writing/thought-of-the-day/index.markdown" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/title.html" defaultContext
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls
            >>= deIndexUrls

    idRouteCopy "css/**"
    idRouteCopy "writing/**/*.png"
    idRouteCopy "writing/**/*.jpg"
    match "templates/*" $ compile templateCompiler

idRouteCopy pattern = match pattern $ do
    route $ idRoute
    compile $ copyFileCompiler

deIndexUrls :: Item String -> Compiler (Item String)
deIndexUrls item =
    if exportToWebserver
        then return $ fmap (withUrls stripIndexHtml) item
        else return item

stripIndexHtml :: String -> String
stripIndexHtml url =
    if "index.html" `isSuffixOf` url && (head url) `elem` "/."
        then take (length url - 10) url
        else url
