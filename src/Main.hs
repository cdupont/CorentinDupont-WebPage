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
  match "bibliography/*.bib"                              $ compile biblioCompiler
  match "pages/*.csl"                                     $ compile cslCompiler
  -- Compile templates
  match ("templates/*.html" .||. "blog/templates/*.html") $ compile templateCompiler
  -- Compile partial markdown (will be inserted in HTML pages)
  match ("blog/*.md" .||. "blurbs/*.md" .||. "news/*.md") $ compile pandocCompiler 
  -- Compile full pages markdown
  match ("pages/*.md" .||. "projects/*.md") $ do
     route $ setExtension ".html"
     compile $ bibtexCompiler >>= mainTemplate
  compileCSS
  -- compile HTML pages
  match ("index.html" .||. "pages/news.html") $ do
      route idRoute
      compile $ getResourceBody >>= mainTemplate
  -- Côpy over static files
  match ("images/*" .||. "js/*" .||. "docs/*.pdf" .||. "bibliography/files/*" .||. "**/img/*") $ do
          route idRoute
          compile copyFileCompiler
  buildPerso

baseContext :: Context String
baseContext =
       dateField  "date"   "%B %e, %Y"
    <> constField "jquery" "//ajax.googleapis.com/ajax/libs/jquery/2.0.3"
    <> defaultContext


-- * Utility functions

-- | Lookup an item based on its file path.
lookupItem :: FilePath -> [Item a] -> Maybe (Item a)
lookupItem path = find ((fromFilePath path ==) . itemIdentifier)

-- | The complete context.
getContext :: Compiler (Context String)
getContext = do
  fileContext <- getBlurbContext
  let newsContext = listField "news" baseContext (loadAll "news/*" >>= recentFirst)
  return (fileContext <> newsContext <> baseContext)

-- | Makes the contents of the blurbs directory available as template fields.
getBlurbContext :: Compiler (Context String)
getBlurbContext = do
    items <- loadAll ("blurbs/*" .||. "blog/*.md")
    let fields = map getField items
    return $ foldr (<>) baseContext fields
  where getField (Item id body) = constField (takeBaseName (toFilePath id)) body

-- | Apply the main template to a page of a given name.
mainTemplate :: Item String -> Compiler (Item String)
mainTemplate item = do
    path <- fmap toFilePath getUnderlying
    context <- fmap (onPage path <>) getContext
    applyAsTemplate context item
      >>= loadAndApplyTemplate "templates/main.html" context
      >>= relativizeUrls
  where onPage path = constField ("on-" ++ (takeBaseName path)) ""

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

