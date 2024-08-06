{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Data.List
import Data.Monoid
import Hakyll
import Network.HTTP.Base
import System.Environment
import System.FilePath
import Text.Pandoc.Options
import Text.Pandoc

main :: IO ()
main = hasHakyllBuildTarget "webserver" >>= hakyllWith config . rules
    where
        hasHakyllBuildTarget :: String -> IO Bool
        hasHakyllBuildTarget target =
            fmap (elem ("HAKYLL_BUILD_TARGET", target))
            getEnvironment

config :: Configuration
config = defaultConfiguration
    { inMemoryCache = False
    }

rules :: Bool -> Rules ()
rules isBuildTargetWebserver = do
    tags <- buildTags patternAllPosts (fromCapture "tags/*/index.html")
    rulesTemplates
    rulesStaticFiles
    rulesTags                              isBuildTargetWebserver tags
    rulesFeeds                             isBuildTargetWebserver
    rulesPageIndexHtmlTemplate             isBuildTargetWebserver
    rulesPageIndexHtml                     isBuildTargetWebserver
    rulesStandalonePageIndexHtml           isBuildTargetWebserver
    rulesPageIndexHtmlTemplateWithoutTitle isBuildTargetWebserver
    rulesPageIndexPandoc                   isBuildTargetWebserver
    rulesPageIndexPandocTemplate           isBuildTargetWebserver tags
    rulesPostIndexHtml                     isBuildTargetWebserver
    rulesPostIndexPandoc                   isBuildTargetWebserver
    rulesPostIndexPandocTocTemplate        isBuildTargetWebserver
    rulesPostIndexPandocNewsletter         isBuildTargetWebserver
    rulesPostIndexUpOneUpPandoc            isBuildTargetWebserver
    rulesPostNamePandoc                    isBuildTargetWebserver

rulesTemplates :: Rules ()
rulesTemplates = do
    match "templates/*" $ do
        compile templateCompiler

rulesStaticFiles :: Rules ()
rulesStaticFiles = do
    match
        (
             "avatar.png"
        .||. "projects/*.gif"
        .||. "projects/**/*.gif"
        .||. "projects/*.png"
        .||. "projects/**/*.png"
        .||. "pages/**/*.png"
        .||. "static/**"
        .||. "writing/**/*.jpg"
        .||. "writing/**/*.png"
        .||. "writing/**/*.gif"
        .||. "writing/alan-kay-notes/*.pdf"
        .||. "writing/rlmeta-poster-2/*.zip"
        ) $ do
        route idRoute
        compile copyFileCompiler

rulesTags :: Bool -> Tags -> Rules ()
rulesTags isBuildTargetWebserver tags = do
    tagsRules tags $ \tag pattern -> do
        let context = contextBase isBuildTargetWebserver
                      `mappend`
                      constField "title" ("Posts tagged " ++ tag)
                      `mappend`
                      listField "posts" (contextPost isBuildTargetWebserver) (filterM (isNotDraftOrDraftPage tag) =<< recentFirst =<< loadAll pattern)
        route idRoute
        compile $ makeItem ""
            >>= loadAndApplyTemplate "templates/tag.html" context
            >>= loadAndApplyTemplate "templates/title.html" context
            >>= loadAndApplyTemplate "templates/default.html" context
            >>= processUrls isBuildTargetWebserver
        feedVersion "rss" tag pattern renderRss
        feedVersion "atom" tag pattern renderAtom
    where
        feedVersion name tag pattern render = version name $ do
            route $ customRoute $ \identifier ->
                replaceFileName (toFilePath identifier) (name ++ ".xml")
            compile $ loadAllSnapshots pattern "postContentOnly"
                >>= filterM (isNotDraftOrDraftPage tag)
                >>= fmap (take 15) . recentFirst
                >>= render
                    (feedConfiguration $ "latest posts tagged " ++ tag)
                    (contextFeed isBuildTargetWebserver)

rulesFeeds :: Bool -> Rules ()
rulesFeeds isBuildTargetWebserver = do
    create ["atom.xml"] $
        process renderAtom
    create ["rss.xml"] $
        process renderRss
    where
        process render = do
            route idRoute
            compile $ feedPosts >>= render (feedConfiguration "latest posts") (contextFeed isBuildTargetWebserver)
        feedPosts =
            loadAllSnapshots patternAllPosts "postContentOnly"
            >>= filterM isNotDraft
            >>= recentFirst
            >>= return . (take 15)

isNotDraftOrDraftPage tag item = do
    if tag == "draft"
        then return True
        else isNotDraft item

isNotDraft item = do
    tags <- getTags (itemIdentifier item)
    return $ not ("draft" `elem` tags)

rulesPageIndexHtmlTemplate :: Bool -> Rules ()
rulesPageIndexHtmlTemplate isBuildTargetWebserver = do
    match "projects/index.html" $
        process $ contextBase isBuildTargetWebserver
    match "projects/timeline/index.html" $
        process $ contextRelatedPosts isBuildTargetWebserver "timeline"
    match "projects/rlselect/index.html" $
        process $ contextRelatedPosts isBuildTargetWebserver "rlselect"
    match "projects/rliterate/index.html" $
        process $ contextRelatedPosts isBuildTargetWebserver "rliterate"
    match "projects/rlmeta/index.html" $
        process $ contextRelatedPosts isBuildTargetWebserver "rlmeta"
    match "projects/agdpp/index.html" $
        process $ contextRelatedPosts isBuildTargetWebserver "agdpp"
    match "projects/rlvideo/index.html" $
        process $ contextRelatedPosts isBuildTargetWebserver "rlvideo"
    where
        process context = do
            route idRoute
            compile $ getResourceBody
                >>= applyAsTemplate context
                >>= loadAndApplyTemplate "templates/title.html" context
                >>= loadAndApplyTemplate "templates/default.html" context
                >>= processUrls isBuildTargetWebserver

rulesPageIndexHtml :: Bool -> Rules ()
rulesPageIndexHtml isBuildTargetWebserver = do
    match "projects/rliterate/book/index.html" $
        process $ contextRecentPosts isBuildTargetWebserver
    match "projects/rliterate/book2/index.html" $
        process $ contextRecentPosts isBuildTargetWebserver
    where
        process context = do
            route idRoute
            compile $ getResourceBody
                >>= loadAndApplyTemplate "templates/default.html" context
                >>= processUrls isBuildTargetWebserver

rulesStandalonePageIndexHtml :: Bool -> Rules ()
rulesStandalonePageIndexHtml isBuildTargetWebserver = do
    match "pages/*/index.html" $
        process $ contextRecentPosts isBuildTargetWebserver
    where
        process context = do
            route idRoute
            compile $ getResourceBody
                >>= loadAndApplyTemplate "templates/standalone.html" context
                >>= processUrls isBuildTargetWebserver

rulesPageIndexHtmlTemplateWithoutTitle :: Bool -> Rules ()
rulesPageIndexHtmlTemplateWithoutTitle isBuildTargetWebserver = do
    match "index.html" $
        process $ contextRecentPosts isBuildTargetWebserver
    where
        process context = do
            route idRoute
            compile $ getResourceBody
                >>= applyAsTemplate context
                >>= loadAndApplyTemplate "templates/default.html" context
                >>= processUrls isBuildTargetWebserver

rulesPageIndexPandoc :: Bool -> Rules ()
rulesPageIndexPandoc isBuildTargetWebserver = do
    match "contact/index.markdown" $ do
        route $ setExtension "html"
        compile $ myPandocCompiler
            >>= loadAndApplyTemplate "templates/title.html" context
            >>= loadAndApplyTemplate "templates/default.html" context
            >>= processUrls isBuildTargetWebserver
    match "ideas/index.markdown" $ do
        route $ setExtension "html"
        compile $ myPandocCompiler
            >>= loadAndApplyTemplate "templates/title.html" context
            >>= loadAndApplyTemplate "templates/default.html" context
            >>= processUrls isBuildTargetWebserver
    where
        context = contextBase isBuildTargetWebserver

rulesPageIndexPandocTemplate :: Bool -> Tags -> Rules ()
rulesPageIndexPandocTemplate isBuildTargetWebserver tags = do
    match "writing/index.markdown" $
        process $
            contextPosts isBuildTargetWebserver recentFirst
            [("posts", patternAllPosts)]
            `mappend`
            (field "tagcloud" $ \_ -> renderTagCloud 80 140 tags)
    match "writing/reflections-on-programming/index.markdown" $
        process $ contextPosts isBuildTargetWebserver chronological
                [("posts", patternReflectionsOnProgramming)]
    match "writing/thought-of-the-day/index.markdown" $
        process $ contextPosts isBuildTargetWebserver chronological
                [ ("thoughts", patternThoughtOfTheDay1)
                , ("thoughts2", patternThoughtOfTheDay2)
                ]
    match "writing/newsletter/index.markdown" $
        process $ contextPosts isBuildTargetWebserver recentFirst
                [ ("newsletters", patternNewsletter)
                ]
    where
        process context = do
            route $ setExtension "html"
            compile $ getResourceBody
                >>= applyAsTemplate context
                >>= renderPandoc
                >>= loadAndApplyTemplate "templates/title.html" context
                >>= loadAndApplyTemplate "templates/default.html" context
                >>= processUrls isBuildTargetWebserver

rulesPostIndexHtml :: Bool -> Rules ()
rulesPostIndexHtml isBuildTargetWebserver = do
    match patternPostIndexHtml $ do
        route idRoute
        compile $ getResourceBody
            >>= loadAndApplyTemplate "templates/title.html" context
            >>= saveSnapshot "postContentOnly"
            >>= loadAndApplyTemplate "templates/default.html" context
            >>= processUrls isBuildTargetWebserver
    where
        context = contextBase isBuildTargetWebserver

rulesPostIndexPandoc :: Bool -> Rules ()
rulesPostIndexPandoc isBuildTargetWebserver = do
    match patternPostIndexPandoc $ do
        route $ setExtension "html"
        compile $ myPandocCompiler
            >>= loadAndApplyTemplate "templates/title.html" context
            >>= saveSnapshot "postContentOnly"
            >>= loadAndApplyTemplate "templates/default.html" context
            >>= processUrls isBuildTargetWebserver
    where
        context = contextPost isBuildTargetWebserver

rulesPostIndexPandocTocTemplate :: Bool -> Rules ()
rulesPostIndexPandocTocTemplate isBuildTargetWebserver = do
    match patternPostIndexPandocTocTemplate $ do
        route $ setExtension "html"
        compile $ myPandocCompilerToc
            >>= loadAndApplyTemplate "templates/title.html" context
            >>= saveSnapshot "postContentOnly"
            >>= loadAndApplyTemplate "templates/default.html" context
            >>= processUrls isBuildTargetWebserver
    where
        context = contextPost isBuildTargetWebserver

rulesPostIndexPandocNewsletter :: Bool -> Rules ()
rulesPostIndexPandocNewsletter isBuildTargetWebserver = do
    match patternPostIndexPandocNewsletter $ do
        route $ setExtension "html"
        compile $ myPandocCompiler
            >>= loadAndApplyTemplate "templates/title.html" context
            >>= saveSnapshot "postContentOnly"
            >>= loadAndApplyTemplate "templates/default.html" context
            >>= processUrls isBuildTargetWebserver
    where
        context = contextPost isBuildTargetWebserver

rulesPostIndexUpOneUpPandoc :: Bool -> Rules ()
rulesPostIndexUpOneUpPandoc isBuildTargetWebserver = do
    match patternThoughtOfTheDay1 $ process
    match patternThoughtOfTheDay2 $ process
    where
        process = do
            route upIndex
            compile $ myPandocCompiler
                >>= loadAndApplyTemplate "templates/title.html" context
                >>= saveSnapshot "postContentOnly"
                >>= loadAndApplyTemplate "templates/default.html" context
                >>= processUrls isBuildTargetWebserver
        context = contextPost isBuildTargetWebserver
        upIndex = customRoute (\identifier ->
            let filePath = toFilePath identifier
            in  (takeDirectory . takeDirectory) filePath
                </> takeBaseName filePath
                </> "index.html")

rulesPostNamePandoc :: Bool -> Rules ()
rulesPostNamePandoc isBuildTargetWebserver = do
    match patternReflectionsOnProgramming $ process
    where
        process = do
            route routeIndex
            compile $ myPandocCompiler
                >>= loadAndApplyTemplate "templates/title.html" (contextPost isBuildTargetWebserver)
                >>= saveSnapshot "postContentOnly"
                >>= loadAndApplyTemplate "templates/default.html" (contextBase isBuildTargetWebserver)
                >>= processUrls isBuildTargetWebserver
        routeIndex = customRoute (\identifier ->
            let filePath = toFilePath identifier
            in  takeDirectory filePath
                </> takeBaseName filePath
                </> "index.html")

patternAllPosts :: Pattern
patternAllPosts =
         patternThoughtOfTheDay1
    .||. patternThoughtOfTheDay2
    .||. patternReflectionsOnProgramming
    .||. patternPostIndexHtml
    .||. patternPostIndexPandoc
    .||. patternPostIndexPandocTocTemplate
    .||. patternPostIndexPandocNewsletter

patternNewsletter :: Pattern
patternNewsletter =
    "writing/newsletter/*/index.markdown"

patternThoughtOfTheDay1 :: Pattern
patternThoughtOfTheDay1 =
    "writing/thought-of-the-day/thoughts/*.markdown"

patternThoughtOfTheDay2 :: Pattern
patternThoughtOfTheDay2 =
    "writing/thought-of-the-day/thoughts2/*.markdown"

patternReflectionsOnProgramming :: Pattern
patternReflectionsOnProgramming =
    "writing/reflections-on-programming/2*.markdown"

patternPostIndexHtml :: Pattern
patternPostIndexHtml =
    "writing/*/index.html"

patternPostIndexPandoc :: Pattern
patternPostIndexPandoc =
         "writing/xmodmap-on-fedora/index.markdown"
    .||. "writing/python-danger-implicit-if/index.markdown"
    .||. "writing/problem-in-commit-message/index.markdown"
    .||. "writing/search-and-replace-in-vim/index.markdown"
    .||. "writing/tell-dont-ask-example/index.markdown"
    .||. "writing/bitten-by-python-generators/index.markdown"
    .||. "writing/evolution-recalling-bash-history/index.md"
    .||. "writing/new-home-for-timeline/index.markdown"
    .||. "writing/alan-kay-notes/index.markdown"
    .||. "writing/atomic-habits-two-minute-rule/index.markdown"
    .||. "writing/bullet-journal-method-migration-as-review/index.markdown"
    .||. "writing/kinesis-advantage-2-swedish-setup/index.markdown"
    .||. "writing/what-is-programming/index.markdown"
    .||. "writing/agdpp-*/index.markdown"
    .||. "writing/devlog-*/index.markdown"
    .||. "writing/fast-feedback-on-graphical-code/index.markdown"
    .||. "writing/high-level-low-level-ron-reply/index.markdown"
    .||. "writing/trying-rons-python-asteroids/index.markdown"
    .||. "writing/how-to-test-a-router/index.markdown"
    .||. "writing/output-tracking-vs-mocks/index.markdown"
    .||. "writing/writing-my-own-video-editor/index.markdown"
    .||. "writing/what-is-a-user-story/index.markdown"
    .||. "writing/function-to-oop-refactoring/index.markdown"
    .||. "writing/ardour-latency-free-overdubbing/index.markdown"

patternPostIndexPandocTocTemplate :: Pattern
patternPostIndexPandocTocTemplate =
         "writing/rlmeta-poster-2/index.markdown"
    .||. "writing/reliable-socket-servers/index.markdown"
    .||. "writing/evolving-design-of-projectional-editor/index.markdown"
    .||. "writing/what-should-a-ci-server-do/index.markdown"
    .||. "writing/tdd-fake-it/index.markdown"

patternPostIndexPandocNewsletter :: Pattern
patternPostIndexPandocNewsletter =
         patternNewsletter

contextRelatedPosts :: Bool -> String -> Context String
contextRelatedPosts isBuildTargetWebserver tagName =
    contextPosts isBuildTargetWebserver filterPosts [("relatedPosts", patternAllPosts)]
    where
        filterPosts patternAllPosts = filterM isRelated patternAllPosts >>= recentFirst
        isRelated item = do
            tags <- getTags (itemIdentifier item)
            return $ tagName `elem` tags

contextRecentPosts :: Bool -> Context String
contextRecentPosts isBuildTargetWebserver =
    contextPosts
        isBuildTargetWebserver
        (fmap (take 5) . recentFirst)
        [("posts", patternAllPosts)]

contextPosts :: Bool -> ([Item String] -> Compiler [Item String]) -> [(String, Pattern)] -> Context String
contextPosts isBuildTargetWebserver transformItems =
    foldr (mappend . createPostsListField) context
    where
        context = contextPost isBuildTargetWebserver
        createPostsListField (fieldName, pattern) =
            listField fieldName context $ loadAll pattern >>= transformItems

contextFeed :: Bool -> Context String
contextFeed isBuildTargetWebserver =
    contextPost isBuildTargetWebserver
    `mappend`
    bodyField "description"

contextPost :: Bool -> Context String
contextPost isBuildTargetWebserver =
    dateField "date" "%e %B %Y"
    `mappend`
    (contextBase isBuildTargetWebserver)

contextBase :: Bool -> Context String
contextBase isBuildTargetWebserver =
    if isBuildTargetWebserver
        then
            field "url" (fmap (maybe "" (stripIndexHtml . toUrl)) . getRoute . itemIdentifier)
            `mappend`
            defaultContext
        else defaultContext
    where
        defaultContext =
            bodyField     "body"     `mappend`
            metadataField            `mappend`
            urlField      "url"      `mappend`
            pathField     "path"     `mappend`
            missingField

feedConfiguration :: String -> FeedConfiguration
feedConfiguration text = FeedConfiguration
    { feedTitle       = "Rickard's personal homepage: " ++ text
    , feedDescription = "Rickard's personal homepage: " ++ text
    , feedAuthorName  = "Rickard Lindberg"
    , feedAuthorEmail = "rickard@rickardlindberg.me"
    , feedRoot        = "http://rickardlindberg.me"
    }

processUrls :: Bool -> Item String -> Compiler (Item String)
processUrls isBuildTargetWebserver x =
    if isBuildTargetWebserver
        then relativizeUrls x >>= deIndexUrls
        else relativizeUrls x

deIndexUrls :: Item String -> Compiler (Item String)
deIndexUrls item = return $ fmap (withUrls stripIndexHtml) item

stripIndexHtml :: String -> String
stripIndexHtml url =
    if "index.html" `isSuffixOf` url && (head url) `elem` ("/." :: String)
        then take (length url - 10) url
        else url

myPandocCompiler :: Compiler (Item String)
myPandocCompiler =
    pandocCompilerWith defaultHakyllReaderOptions defaultHakyllWriterOptions
        { writerEmailObfuscation = JavascriptObfuscation
        }

myPandocCompilerToc :: Compiler (Item String)
myPandocCompilerToc =
    pandocCompilerWith defaultHakyllReaderOptions defaultHakyllWriterOptions
        { writerEmailObfuscation = JavascriptObfuscation
        , writerTableOfContents = True
        , writerTOCDepth = 3
        , writerTemplate = Just tocTemplate
        }
    where
        tocTemplate = either error id
            $ either (error . show) id
            $ runPure
            $ runWithDefaultPartials
            $ compileTemplate "" "$toc$\n$body$"
