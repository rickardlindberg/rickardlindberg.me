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

    match "templates/*" $ compile templateCompiler
