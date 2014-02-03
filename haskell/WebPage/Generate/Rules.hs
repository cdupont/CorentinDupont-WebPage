{-# LANGUAGE OverloadedStrings #-}

module WebPage.Generate.Rules (rules) where

import System.FilePath

import Hakyll
import           Data.Monoid     ((<>), mconcat)
import WebPage.Generate.Base
import WebPage.Generate.Context
import WebPage.Pubs
import qualified Text.Pandoc     as Pandoc
--
-- * Exported functions

rules = do
  compileTemplates
  compileMarkdown
  compileCSS
  copyFiles
  loadAbstracts
  buildPages
  buildPerso

-- * Internal functions

compileTemplates :: Rules ()
compileTemplates =
  match ("templates/*.html" .||. "blog/templates/*.html") $
    compile templateCompiler

compileMarkdown :: Rules ()
compileMarkdown =
  match ("blurbs/*.md" .||. "news/*.md" .||. "blog/*.md") $
    compile pandocCompiler


compileCSS :: Rules ()
compileCSS = do
  match "css/*.less" $ do
    compile getResourceBody
    less <- makePatternDependency "css/*.less"
    rulesExtraDependencies [less] $ create ["css/all.css"] $ do
      route idRoute
      compile $ loadBody "css/all.less"
        >>= makeItem
        >>= withItemBody (unixFilter "lessc" ["-"])
        >>= return . fmap compressCss
  match "css/*.css" $ do
    route idRoute
    compile compressCssCompiler

copyFiles :: Rules ()
copyFiles =
  match ("images/*" .||. "js/*" .||. "docs/*.pdf") $ do
    route   idRoute
    compile copyFileCompiler

loadAbstracts :: Rules ()
loadAbstracts =
  match "docs/*.abstract.md" $
    compile pandocCompiler

buildPages :: Rules()
buildPages = do
    match "pages/*" $ do
      route (customRoute (flip addExtension "html" . takeBaseName . toFilePath))
      compilePage
    match "projects/*" $ do
      route (customRoute (flip addExtension "html" . dropExtension . toFilePath))
      compilePage
  where
    compilePage = compile $ do
      path <- fmap toFilePath getUnderlying
      let content = case takeExtension path of
            ".html" -> getResourceBody
            ".md"   -> pandocCompiler
            _       -> error ("Unexpected file type: " ++ path)
      content >>= mainTemplate (takeBaseName path)

buildPerso :: Rules ()
buildPerso = do
    -- Compress CSS
    match "blog/css/*" $ do
        route idRoute
        compile compressCssCompiler

    -- Render the /tmp index page
    match "tmp/index.html" $ do
        route idRoute
        compile $ getResourceBody >>= relativizeUrls

    -- Build tags
    tags <- buildTags "blog/posts/*" (fromCapture "tags/*.html")

    -- Render each and every post
    match ("blog/posts/*.md" .||. "blog/posts/*.markdown" .||. "blog/posts/*.lhs" .||.
           "blog/drafts/*.md" .||. "blog/drafts/*.markdown" .||. "blog/drafts/*.lhs") $ do
        route   $ setExtension ".html"
        compile $ do
            pandocCompiler
                >>= saveSnapshot "content"
                >>= return . fmap demoteHeaders
                >>= loadAndApplyTemplate "blog/templates/post.html" (postCtx tags)
                >>= loadAndApplyTemplate "blog/templates/mainperso.html" defaultContext
                >>= relativizeUrls

    -- Post list
    create ["blog/posts.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "blog/posts/*"
            let ctx = constField "title" "Posts" <>
                        listField "posts" (postCtx tags) (return posts) <>
                        defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "blog/templates/posts.html" ctx
                >>= loadAndApplyTemplate "blog/templates/mainperso.html" ctx
                >>= relativizeUrls

    -- Post tags
    tagsRules tags $ \tag pattern -> do
        let title = "Posts tagged " ++ tag

        -- Copied from posts, need to refactor
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            let ctx = constField "title" title <>
                        listField "posts" (postCtx tags) (return posts) <>
                        defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "blog/templates/posts.html" ctx
                >>= loadAndApplyTemplate "blog/templates/mainperso.html" ctx
                >>= relativizeUrls

        -- Create RSS feed as well
        version "rss" $ do
            route   $ setExtension "xml"
            compile $ loadAllSnapshots pattern "content"
                >>= fmap (take 10) . recentFirst
                >>= renderAtom (feedConfiguration title) feedCtx

    -- Index
    match "blog/index.html" $ do
        route idRoute
        compile $ do
            posts <- fmap (take 3) . recentFirst =<< loadAll "blog/posts/*"
            context <-  getContext
            let indexContext =
                    listField "posts" (postCtx tags) (return posts) <>
                    field "tags" (\_ -> renderTagList tags) <>
                    context <>
                    defaultContext
            getResourceBody
                >>= applyAsTemplate indexContext
                >>= loadAndApplyTemplate "blog/templates/mainperso.html" indexContext
                >>= relativizeUrls

    -- Read templates
    match "blog/templates/*.html" $ compile $ templateCompiler

    -- Render some static pages
    match (fromList pages) $ do
        route   $ setExtension ".html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "blog/templates/mainperso.html" defaultContext
            >>= relativizeUrls

    -- Render the 404 page, we don't relativize URL's here.
    match "404.html" $ do
        route idRoute
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "blog/templates/mainperso.html" defaultContext

  where
    pages =
        [ "blog/contact.md"
        , "blog/recommendations.md"
        ]




--------------------------------------------------------------------------------
postCtx :: Tags -> Context String
postCtx tags = mconcat
    [ modificationTimeField "mtime" "%U"
    , dateField "date" "%B %e, %Y"
    , tagsField "tags" tags
    , defaultContext
    ]


--------------------------------------------------------------------------------
feedCtx :: Context String
feedCtx = mconcat
    [ bodyField "description"
    , defaultContext
    ]



--------------------------------------------------------------------------------
feedConfiguration :: String -> FeedConfiguration
feedConfiguration title = FeedConfiguration
    { feedTitle       = "Corentin Dupont - " ++ title
    , feedDescription = "Personal blog of Corentin Dupont"
    , feedAuthorName  = "Corentin Dupont"
    , feedAuthorEmail = "corentin.dupont@gmail.com"
    , feedRoot        = "http://corentindupont.info"
    }

