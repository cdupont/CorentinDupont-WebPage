{-# LANGUAGE OverloadedStrings #-}

module WebPage.Generate.Rules (rules) where

import System.FilePath

import Hakyll
import Data.Monoid     ((<>), mconcat)
import qualified Data.Set as S
import WebPage.Generate.Base
import WebPage.Generate.Context
import WebPage.Pubs
import Control.Applicative
import qualified Text.Pandoc     as Pandoc
import Text.Pandoc.Options
import           Data.List                       (intercalate, intersperse, sortBy)

import           Text.Blaze.Html                 (toHtml, toValue, (!))
import           Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5                as H
import qualified Text.Blaze.Html5.Attributes     as A
--
-- * Exported functions

rules = do
  compileTemplates
  compileMarkdown
  compileCSS
  copyFiles
  buildPages
  buildPerso

-- * Internal functions

compileTemplates :: Rules ()
compileTemplates = match ("templates/*.html" .||. "blog/templates/*.html") $ compile templateCompiler

compileMarkdown :: Rules ()
compileMarkdown = match ("blurbs/*.md" .||. "news/*.md" .||. "blog/*.md") $ compile pandocCompiler


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
  match ("css/*.css" .||. "blog/css/*") $ do
    route idRoute
    compile compressCssCompiler

copyFiles :: Rules ()
copyFiles =
  match ("images/*" .||. "js/*" .||. "docs/*.pdf" .||. "blog/posts/figure/*") $ do
    route   idRoute
    compile copyFileCompiler

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

    -- Build tags
    tags <- buildTags       "blog/posts/**/*" (fromCapture "tags/*.html")
    cats <- buildCategories "blog/posts/**/*" (fromCapture "tags/*.html")

    -- build post lists
    tagsRules tags $ postList tags cats
    tagsRules cats $ postList tags cats
    create ["blog/posts.html"] $ postList tags cats "All posts" "blog/posts/**/*"

    -- Render each and every post
    match ("blog/posts/**/*.md" .||. "blog/posts/**/*.lhs" .||."blog/drafts/*") $ do
        route   $ setExtension ".html"
        compile $ do
            pandocMathCompiler
                >>= saveSnapshot "content"
                >>= return . fmap demoteHeaders
                >>= loadAndApplyTemplate "blog/templates/post.html" (postCtx tags cats)
                >>= loadAndApplyTemplate "blog/templates/mainperso.html" (postCtx tags cats)
                >>= relativizeUrls


    -- build index page
    match "blog/index.html" $ do
        route idRoute
        compile $ do
            posts <- fmap (take 10) . recentFirst =<< loadAll "blog/posts/**/*"
            context <- getContext
            let indexContext =
                    listField "posts" (postCtx tags cats) (return posts) <>
                    field "tags" (\_ -> renderTagList tags) <>
                    field "cats" (\_ -> renderCatsList cats) <>
                    context <>
                    defaultContext
            getResourceBody
                >>= applyAsTemplate indexContext
                >>= loadAndApplyTemplate "blog/templates/mainperso.html" indexContext
                >>= relativizeUrls

    -- Render some static pages
    match "blog/pages/*.md" $ do
        route   $ setExtension ".html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "blog/templates/mainperso.html" (postCtx tags cats)
            >>= relativizeUrls


postList :: Tags -> Tags -> String -> Pattern -> Rules ()
postList tags cats title pattern = do
    route idRoute
    compile $ do
        posts <- recentFirst =<< loadAll pattern
        let ctx = constField "title" title <>
                  listField "posts" (postCtx tags cats) (return posts) <>
                  postCtx tags cats
        makeItem ""
           >>= loadAndApplyTemplate "blog/templates/posts.html" ctx
           >>= loadAndApplyTemplate "blog/templates/mainperso.html" ctx
           >>= relativizeUrls


renderCatsList :: Tags -> Compiler (String)
renderCatsList = renderTags makeLink (intercalate " ")
  where
    makeLink tag url count _ _ = renderHtml $
        H.a ! A.href (toValue url) $ toHtml (tag ++ " (" ++ show count ++ ")")

--------------------------------------------------------------------------------
postCtx :: Tags -> Tags -> Context String
postCtx tags cats = mconcat
    [ modificationTimeField "mtime" "%U"
    , dateField "date" "%B %e, %Y"
    , tagsField "tags" tags
    , field "cats" (\_ -> renderCatsList cats)
    , constField "jquery" "//ajax.googleapis.com/ajax/libs/jquery/2.0.3"
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

pandocMathCompiler =
    let mathExtensions = [Ext_tex_math_dollars, Ext_tex_math_double_backslash,
                          Ext_latex_macros]
        defaultExtensions = writerExtensions defaultHakyllWriterOptions
        newExtensions = foldr S.insert defaultExtensions mathExtensions
        writerOptions = defaultHakyllWriterOptions {
                          writerExtensions = newExtensions,
                          writerHTMLMathMethod = MathJax ""
                        }
    in pandocCompilerWith defaultHakyllReaderOptions writerOptions

