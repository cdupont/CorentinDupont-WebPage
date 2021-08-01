{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.List                       (intercalate, intersperse, sortBy, find)
import           Data.Monoid                     (mconcat, (<>))
import qualified Data.Set                        as S
import           Debug.Trace
import           Hakyll
import           Hakyll.Web.Pandoc.Biblio
import           System.FilePath ((<.>), (</>), pathSeparator)
import           Text.Blaze.Html                 (toHtml, toValue, (!))
import           Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5                as H
import qualified Text.Blaze.Html5.Attributes     as A
import           Text.Pandoc                     as Pandoc
import           Text.Pandoc.Options
import           Text.Pandoc.R
import           System.FilePath

main = hakyll $ do 
  -- Compile bibliography
  match "bibliography/*.bib"  $ compile biblioCompiler
  match "pages/*.csl"         $ compile cslCompiler
  -- Compile templates
  match "templates/*.html"    $ compile templateCompiler
  -- Compile partial markdown (will be inserted in HTML pages)
  match ("news/*.md") $ compile pandocCompiler 
  -- Compile markdown pages
  match ("pages/*.md") $ do
     route $ setExtension ".html"
     compile $ bibtexCompiler >>=
         loadAndApplyTemplate "templates/main.html" (baseContext)
         >>= relativizeUrls
  compileCSS
  -- compile index page
  match ("index.html") $ do
      route idRoute
      compile $ do
         posts <- fmap (take 3) . recentFirst =<< loadAll "news/*"
         let indexContext = 
                listField "news" baseContext (return posts) <>
                baseContext
         getResourceBody
            >>= applyAsTemplate indexContext
            >>= loadAndApplyTemplate "templates/main.html" indexContext
            >>= relativizeUrls

  -- Post list
  create ["pages/news.html"] $ do
      route idRoute
      compile $ do
          posts <- recentFirst =<< loadAll "news/*"
          let ctx = constField "title" "News" <>
                      listField "news" baseContext (return posts) <>
                      baseContext
          makeItem ""
              >>= loadAndApplyTemplate "templates/news.html" ctx
              >>= loadAndApplyTemplate "templates/main.html" ctx
              >>= relativizeUrls

  let posts = ("blog/posts/*/*.md" .||. "blog/posts/*/*.lhs" .||. "blog/posts/*/*.Rmd")
  let drafts = "blog/drafts/*"
  -- Build tags
  tags <- buildTags       posts (fromCapture "tags/*.html")
  cats <- buildCategories posts (fromCapture "tags/*.html")

  -- build post lists
  tagsRules tags $ postList tags cats
  tagsRules cats $ postList tags cats
  create ["blog/posts.html"] $ postList tags cats "All posts" posts

  -- Render each post
  match (posts .||. drafts) $ do
      route   $ setExtension ".html"
      compile $ do
          myPandocCompiler
              >>= saveSnapshot "content"
              >>= return . fmap demoteHeaders
              >>= loadAndApplyTemplate "templates/post.html" (postCtx tags cats)
              >>= loadAndApplyTemplate "templates/mainperso.html" (postCtx tags cats)
              >>= relativizeUrls

  -- build blog page
  match "blog/index.html" $ do
      route idRoute
      compile $ do
          posts <- fmap (take 10) . recentFirst =<< loadAll posts
          let indexContext =
                  listField "posts" (postCtx tags cats) (return posts) <>
                  field "tags" (\_ -> renderTagList tags) <>
                  field "cats" (\_ -> renderCatsList cats) <>
                  baseContext <>
                  defaultContext
          getResourceBody
              >>= applyAsTemplate indexContext
              >>= loadAndApplyTemplate "templates/mainperso.html" indexContext
              >>= relativizeUrls

  -- Render some static pages
  match "blog/pages/*.md" $ do
        route   $ setExtension ".html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/mainperso.html" (postCtx tags cats)
            >>= relativizeUrls

  -- Côpy over static files
  match ("images/*" .||. "js/*" .||. "docs/*.pdf" .||. "bibliography/files/*" .||. "**/img/*") $ do
          route idRoute
          compile copyFileCompiler

baseContext :: Context String
baseContext =
       dateField  "date"   "%B %e, %Y"
    <> constField "jquery" "//ajax.googleapis.com/ajax/libs/jquery/2.0.3"
    <> defaultContext

bibtexCompiler :: Compiler (Item String)
bibtexCompiler = do
  bib <- load "bibliography/central-bibliography.bib"
  csl <- load "pages/inline.csl"
  body <- getResourceBody
  comp <- readPandocBiblio defaultHakyllReaderOptions csl bib body
  return $ writePandoc comp 

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

postList :: Tags -> Tags -> String -> Pattern -> Rules ()
postList tags cats title pattern = do
    route idRoute
    compile $ do
        posts <- recentFirst =<< loadAll pattern
        let ctx = constField "title" title <>
                  listField "posts" (postCtx tags cats) (return posts) <>
                  postCtx tags cats
        makeItem ""
           >>= loadAndApplyTemplate "templates/posts.html" ctx
           >>= loadAndApplyTemplate "templates/mainperso.html" ctx
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


myPandocCompiler :: Compiler (Item String)
myPandocCompiler = pandocCompilerWithTransformM readerOptions writerOptions $ diagramsTransformer >=> rTransformer

writerOptions :: WriterOptions
writerOptions = defaultHakyllWriterOptions {
   writerExtensions = (writerExtensions defaultHakyllWriterOptions) <> extensionsFromList [Ext_tex_math_dollars, Ext_tex_math_double_backslash, Ext_latex_macros, Ext_fenced_code_attributes, Ext_fenced_code_blocks],
   writerHTMLMathMethod = MathJax ""}

readerOptions :: ReaderOptions
readerOptions = defaultHakyllReaderOptions {
   readerExtensions = (readerExtensions defaultHakyllReaderOptions) <> extensionsFromList [Ext_fenced_code_attributes, Ext_fenced_code_blocks, Ext_backtick_code_blocks]}

rTransformer :: Pandoc -> Compiler Pandoc
rTransformer pandoc = unsafeCompiler $ renderRPandoc "images" True pandoc

diagramsTransformer :: Pandoc -> Compiler Pandoc
diagramsTransformer pandoc = return pandoc --unsafeCompiler $ renderBlockDiagrams "./images" True pandoc

--renderBlockDiagrams :: FilePath -> Bool -> Pandoc -> IO Pandoc
--renderBlockDiagrams outDir absolutePath p = bottomUpM (fmap concat . mapM (insertDiagrams (Opts "PNG" outDir "example" absolutePath))) p 

