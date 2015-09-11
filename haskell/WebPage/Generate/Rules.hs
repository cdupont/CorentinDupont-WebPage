{-# LANGUAGE OverloadedStrings #-}

module WebPage.Generate.Rules (rules) where

import           Control.Applicative
import           Control.Monad
import           Data.List                       (intercalate, intersperse,
                                                  sortBy)
import           Data.Monoid                     (mconcat, (<>))
import qualified Data.Set                        as S
import           Hakyll
import           Hakyll.Web.Pandoc.Biblio
import           System.FilePath
import           Text.Blaze.Html                 (toHtml, toValue, (!))
import           Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5                as H
import qualified Text.Blaze.Html5.Attributes     as A
import           Text.Pandoc                     as Pandoc
import           Text.Pandoc.Diagrams
import           Text.Pandoc.Options
import           Text.Pandoc.R
import           WebPage.Generate.Base
import           WebPage.Generate.Context

rules = do
  compileBibtex
  compileTemplates
  compileMarkdown
  compileCSS
  copyFiles
  buildPro
  buildPages
  buildPerso


-- * Internal functions

compileTemplates :: Rules ()
compileTemplates = match ("templates/*.html" .||. "blog/templates/*.html") $ compile templateCompiler

compileMarkdown :: Rules ()
compileMarkdown = match ("blurbs/*.md" .||. "news/*.md" .||. "blog/*.md") $ compile pandocCompiler

compileBibtex :: Rules ()
compileBibtex = do
  match "bibliography/*.bib" $ compile $ biblioCompiler
  match "pages/*.csl" $ compile $ cslCompiler

bibtexCompiler :: Compiler (Item String)
bibtexCompiler = do
  bib <- load "bibliography/central-bibliography.bib"
  csl <- load "pages/inline.csl"
  getResourceBody
    >>= readPandocBiblio def csl bib
    >>= return . writePandoc

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
  match ("images/*" .||. "js/*" .||. "docs/*.pdf") $ do
    route idRoute
    compile copyFileCompiler

buildPro :: Rules()
buildPro = do
    match ("index.html" .||. "pages/news.html") $ do
      route idRoute
      compile $ getResourceBody >>= mainTemplate

buildPages :: Rules()
buildPages = do
    match ("pages/*.md" .||. "projects/*.md") $ do
      route $ setExtension ".html"
      compile $ bibtexCompiler >>= mainTemplate

buildPerso :: Rules ()
buildPerso = do
    let posts = ("blog/posts/*/*.md" .||. "blog/posts/*/*.lhs" .||. "blog/posts/*/*.Rmd")
    let drafts = "blog/drafts/*"
    -- Build tags
    tags <- buildTags       posts (fromCapture "tags/*.html")
    cats <- buildCategories posts (fromCapture "tags/*.html")

    -- build post lists
    tagsRules tags $ postList tags cats
    tagsRules cats $ postList tags cats
    create ["blog/posts.html"] $ postList tags cats "All posts" posts

    -- Render each and every post
    match (posts .||. drafts) $ do
        route   $ setExtension ".html"
        compile $ do
            myPandocCompiler
                >>= saveSnapshot "content"
                >>= return . fmap demoteHeaders
                >>= loadAndApplyTemplate "blog/templates/post.html" (postCtx tags cats)
                >>= loadAndApplyTemplate "blog/templates/mainperso.html" (postCtx tags cats)
                >>= relativizeUrls

    -- build index page
    match "blog/index.html" $ do
        route idRoute
        compile $ do
            posts <- fmap (take 10) . recentFirst =<< loadAll posts
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
renderCatsList tags = renderTags makeLink (intercalate " ") (sortTagsBy sortOther tags)
  where
    ("Others", _) `sortOther` (_, _) = GT
    (_, _) `sortOther` ("Others", _) = LT
    a `sortOther` b = compare a b
    makeLink tag url count _ _ = renderHtml $ H.a ! A.href (toValue url) $ toHtml tag

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

myPandocCompiler :: Compiler (Item String)
myPandocCompiler = pandocCompilerWithTransformM readerOptions writerOptions $ (diagramsTransformer "images") >=> (rTransformer "images")

writerOptions :: WriterOptions
writerOptions = defaultHakyllWriterOptions {
   writerExtensions = foldr S.insert (writerExtensions defaultHakyllWriterOptions) [Ext_tex_math_dollars, Ext_tex_math_double_backslash, Ext_latex_macros, Ext_fenced_code_attributes, Ext_fenced_code_blocks],
   writerHTMLMathMethod = MathJax ""}

readerOptions :: ReaderOptions
readerOptions = defaultHakyllReaderOptions {
   readerExtensions = foldr S.insert (readerExtensions defaultHakyllReaderOptions) [Ext_fenced_code_attributes, Ext_fenced_code_blocks, Ext_backtick_code_blocks]}

rTransformer :: FilePath -> Pandoc -> Compiler Pandoc
rTransformer outDir pandoc = unsafeCompiler $ renderRPandoc outDir pandoc

diagramsTransformer :: FilePath -> Pandoc -> Compiler Pandoc
diagramsTransformer outDir pandoc = unsafeCompiler $ renderBlockDiagrams outDir pandoc
