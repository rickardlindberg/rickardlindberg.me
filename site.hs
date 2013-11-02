{-# LANGUAGE OverloadedStrings #-}

import Data.List
import System.FilePath

import Hakyll

exportToWebserver = True

main :: IO ()
main = hakyll $ do
    htmlRoutePandoc "index.markdown"
    htmlRoutePandoc "writing/ardour-latency-free-overdubbing/index.rst"
    idRouteCopy "writing/**/*.png"
    idRouteCopy "writing/**/*.jpg"
    match "templates/*" $ compile templateCompiler

htmlRoutePandoc pattern = match pattern $ do
    route $ setExtension "html"
    compile $ pandocCompiler
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls
        >>= deIndexUrls

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
