module Schnizle.Indexed
  ( indexedRoute
  , indexedRouteWith
  , relativizeIndexed
  ) where

import Data.List (isSuffixOf)
import System.FilePath.Posix

import Hakyll


indexedRouteWith :: String -> Routes
indexedRouteWith prefix = customRoute toIndexed
  where
    toIndexed id' = prefix </> takeDirectory path </> takeBaseName path </> "index.html"
      where
        path = toFilePath id'

indexedRoute :: Routes
indexedRoute = indexedRouteWith ""

relativizeIndexed :: Item String -> Compiler (Item String)
relativizeIndexed = return . fmap (replaceAll "/index.html" (const ""))

