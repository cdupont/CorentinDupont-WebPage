{-# LANGUAGE OverloadedStrings, PatternGuards #-}

module WebPage.Generate.Context (
  getContext,
  mainTemplate
) where

import Data.Monoid ((<>))
import System.FilePath

import WebPage.Generate.Base
import Hakyll
import Debug.Trace

-- * Exported functions

-- | The complete context.
getContext :: Compiler (Context String)
getContext = do
  fileContext <- getBlurbContext
  return (fileContext <> newsContext <> baseContext)


-- | Apply the main template to a page of a given name.
mainTemplate :: Item String -> Compiler (Item String)
mainTemplate item = do
    path <- fmap toFilePath getUnderlying
    context <- fmap (onPage path <>) getContext
    applyAsTemplate context item
      >>= loadAndApplyTemplate "templates/main.html" context
      >>= relativizeUrls
  where onPage path = constField ("on-" ++ (takeBaseName path)) ""


-- * Internal functions

-- ** News context

-- | Add news items to context as a list.
newsContext :: Context String
newsContext = listField "news" baseContext (loadAll "news/*" >>= recentFirst)


-- ** File context

-- | Makes the contents of the blurbs directory available as template fields.
getBlurbContext :: Compiler (Context String)
getBlurbContext = do
    loadAll ("blurbs/*" .||. "blog/*.md")
      >>= return . foldr (<>) baseContext . map item
  where item (Item id body) = constField (takeBaseName (toFilePath id)) body
