module Schnizle.Fields
  ( RelatedLinks(..)
  , buildRelatedLinks
  , relatedLinksField
  , relatedUrlField, relatedTitleField, relatedDateField
  , defaultRelatedContext
  , additionalLinksField
  , nowField
  ) where

import           Control.Arrow

import           Data.Binary
import           Data.Foldable
import           Data.Function
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Typeable

import           Data.Time.Clock       (UTCTime (..), getCurrentTime)
import           Data.Time.Format      (defaultTimeLocale, formatTime)

import           System.FilePath.Posix

import qualified Data.Map              as M
import qualified Data.Sequence         as S

import           Hakyll

import           Control.Monad


-- related links -------------------------------------------------------------
type RelatedTags = M.Map String [Identifier]
type RelatedMap  = M.Map Identifier Related

data Related = Related
  { relatedId    :: Identifier
  , relatedDate  :: UTCTime
  , relatedTitle :: String }
  deriving Show

data RelatedLinks = RelatedLinks RelatedMap RelatedTags
  deriving Show


buildRelatedTags :: [Identifier] -> RelatedTags -> Rules RelatedTags
buildRelatedTags []         tags = return tags
buildRelatedTags (id':ids') tags = buildRelatedTags ids' =<< (foldl addTag tags <$> getTags id')
  where
    addTag tags tag = M.insertWith (++) tag [id'] tags

buildRelatedMap :: [Identifier] -> Rules RelatedMap
buildRelatedMap ids' = foldlM addRelated M.empty ids'
  where
    addRelated :: RelatedMap -> Identifier -> Rules RelatedMap
    addRelated map' id' = do
      date  <- getItemUTC defaultTimeLocale id'
      title <- getMetadataField' id' "title"
      return $ M.insert id' (Related id' date title) map'

buildRelatedLinks :: Pattern -> Rules RelatedLinks
buildRelatedLinks pattern = do
  matches <- getMatches pattern
  RelatedLinks <$> buildRelatedMap matches <*> buildRelatedTags matches M.empty


relatedDateField :: String -> String -> Context Related
relatedDateField name fmt = field name (return . format . relatedDate . itemBody)
  where
    format = formatTime defaultTimeLocale fmt

relatedTitleField :: String -> Context Related
relatedTitleField name = field name (return . relatedTitle . itemBody)

relatedUrlField :: String -> Context Related
relatedUrlField name = field name $ \item -> genUrl $ relatedId $ itemBody item
  where
    genUrl id = maybe "#" toUrl <$> getRoute id

defaultRelatedContext :: Context Related
defaultRelatedContext = relatedTitleField "title"
                     <> relatedUrlField "url"
                     <> relatedDateField "date" "%B %d, %Y"
                     <> missingField

relatedLinksField :: Int -> String -> RelatedLinks -> Context Related -> Context a
relatedLinksField n name (RelatedLinks map' tags') ctx = listFieldWith name ctx $ \item -> do
  needle  <- getTags $ itemIdentifier item
  ids     <- return $ clean item $ findRelatedByTags tags' needle
  related <- return $ fetchRelated map' $ clearIds $ sortByFrequency ids
  return $ map relatedToItem related

  where
    clean item = filter (itemIdentifier item /=)
    clearIds   = take n . nub
    relatedToItem related = Item (relatedId related) related


findRelatedByTags :: RelatedTags -> [String] -> [Identifier]
findRelatedByTags map' tags = concat $ catMaybes $ map (flip M.lookup map') tags

fetchRelated :: RelatedMap -> [Identifier] -> [Related]
fetchRelated map' = catMaybes . map (flip M.lookup map')

sortByFrequency :: [Identifier] -> [Identifier]
sortByFrequency ids = map snd $ sortBy (compare `on` fst) $
  map (length &&& head) (group (sort ids))

-- additional links -----------------------------------------------------------

additionalLinksField :: String -> Context String
additionalLinksField name = listFieldWith name (field "link" (return . itemBody)) $ \item -> do
  meta <- getMetadata (itemIdentifier item)
  mapM makeItem $ allLinks meta
  where
    allLinks m = maybe [] (map trim . splitAll " ") $ M.lookup "adds" m

-- now field ------------------------------------------------------------------
nowField :: String -> String -> Context String
nowField key fmt = field key $ \_ ->
  unsafeCompiler $ (formatTime defaultTimeLocale fmt <$> getCurrentTime)

