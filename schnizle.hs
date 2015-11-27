{-# LANGUAGE OverloadedStrings #-}


import Control.Monad (forM_)
import Data.Monoid
import Data.List

import Hakyll
import Hakyll.Web.Paginate
import Hakyll.Web.Sass

import Schnizle.Haml
import Schnizle.Config
import Schnizle.Indexed
import Schnizle.Fields

postsPerPage :: Int
postsPerPage = 10

main :: IO ()
main = hakyllWith config $ do

  tags  <- buildTags "posts/*.md" $ fromCapture "category/*/index.html"
  pages <- buildBlogPages "posts/*.md"
  related <- buildRelatedLinks "posts/*.md"

  -- prepare templates
  match ("templates/*.haml" .||. "templates/static/*.haml") $ compile hamlCompiler
  match ("templates/*.xml" .||. "templates/*.html") $ compile templateCompiler

  -- render css
  match "assets/css/*.sass" $ do
    route $ setExtension "css"
    compile sassCompiler

  -- copy static asserts
  match ("assets/img/**" .||. "assets/js/**" .||. "assets/css/*.css") $ do
    route idRoute
    compile copyFileCompiler

  -- create blog ----------------------------------------------------------------

  match "posts/*.md" $ do
    route $ indexedRouteWith "blog"
    compile $ pandocCompilerWith defaultHakyllReaderOptions pandocOptions
      >>= loadAndApplyTemplate "templates/post.haml" (postCtx tags related)
      >>= loadAndApplyTemplate "templates/layout.haml" (postCtx tags related)
      >>= relativizeIndexed


  paginateRules pages $ \index pattern -> do
    route idRoute
    compile $ makeItem ""
      >>= loadAndApplyTemplate "templates/blog.haml" (blogCtx index pages tags related)
      >>= loadAndApplyTemplate "templates/layout.haml" (blogCtx index pages tags related)
      >>= relativizeIndexed

  -- sitemap --------------------------------------------------------------------
  create ["sitemap.xml"] $ do
      route idRoute
      compile $ makeItem ""
          >>= loadAndApplyTemplate "templates/sitemap.xml" (sitemapCtx tags related)
          >>= relativizeIndexed

  -- feed ------------------------------------------------------------------------
  create ["feed.xml"] $ do
      route idRoute
      compile $ (recentFirst =<< loadAll "posts/*.md")
          >>= renderRss feedConfig defaultContext

  -- create static pages ---------------------------------------------------------
  create ["index.html"] $ do
    route idRoute
    compile $ makeItem ""
      >>= loadAndApplyTemplate "templates/index.haml" defaultContext
      >>= loadAndApplyTemplate "templates/layout.haml" defaultContext
      >>= relativizeIndexed

  forM_ ["about", "notice", "mail"] $ \file ->
    create [fromFilePath $ file ++ "/index.html"] $ do
      route idRoute
      compile $ makeItem ""
        >>= loadAndApplyTemplate ( fromFilePath $ "templates/static/" ++ file ++ ".haml") defaultContext
        >>= loadAndApplyTemplate "templates/layout.haml" defaultContext
        >>= relativizeIndexed



-- blog --------------------------------------------------------------------------
buildBlogPages :: (MonadMetadata m) => Pattern -> m Paginate
buildBlogPages pattern = buildPaginateWith (return . paginateEvery postsPerPage) pattern $ \index ->
    if index == 1 
       then fromFilePath "blog/index.html"
       else fromFilePath $ "blog/" ++ show index ++ "/index.html"

postCtx :: Tags -> RelatedLinks -> Context String
postCtx tags related = defaultContext
  <> tagsField "tags" tags
  <> constField "keywords" tagList
  <> dateField "date" "%B %d, %Y"
  <> dateField "day"  "%d"
  <> dateField "month" "%m"
  <> dateField "year"  "%Y"
  <> dateField "created" "%Y-%m-%d"
  <> relatedLinksField 2 "related" related defaultRelatedContext
  <> additionalLinksField "links"
  <> modificationTimeField "modified" "%Y-%m-%d"
  where
    tagList = intercalate "," $ map fst $ tagsMap tags

blogCtx :: PageNumber -> Paginate -> Tags -> RelatedLinks -> Context String
blogCtx i pages tags related = defaultContext
  <> constField "title" "what ever comes to mind"
  <> listField "posts" (postCtx tags related) posts
  <> modificationTimeField "modified" "%Y-%m-%d"
  <> paginateContext pages i
    where
      posts = takeFromTo <$> (recentFirst =<< loadAll "posts/*.md")
      takeFromTo = drop start . take end

      start = postsPerPage * (i - 1)
      end   = postsPerPage * i

sitemapCtx :: Tags -> RelatedLinks -> Context String
sitemapCtx tags related = defaultContext
    <> listField "posts" (postCtx tags related) (recentFirst =<< loadAll "posts/*.md")
    <> nowField "created" "%Y-%m-%d"

