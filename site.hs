{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid (mappend)
import System.FilePath

import Hakyll

main :: IO ()
main = hakyll $ do

    match "index.markdown" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "writing/ardour-latency-free-overdubbing/index.rst" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "writing/**/*.png" $ do
        route $ idRoute
        compile $ copyFileCompiler

    match "writing/**/*.jpg" $ do
        route $ idRoute
        compile $ copyFileCompiler

    match "templates/*" $ compile templateCompiler
