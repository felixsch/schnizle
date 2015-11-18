module Schnizle.Fields
  ( relatedPostsField
  , additionalLinksField
  , nowField
  ) where

import Control.Arrow

import Data.List
import Data.Typeable
import Data.Binary 
import Data.Function
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)

import qualified Data.Map as M

import Hakyll


-- related links -------------------------------------------------------------
type Version = String

relatedPostsField :: (Typeable a, Binary a) => String -> Version -> Tags -> Context a -> Context b
relatedPostsField name version tags ctx = listFieldWith name ctx $ \item -> do
  needle <- tagsByItem item 
  loadAll $ toPatternWith version (related item needle)

  where
    related item needle = take 2 $ 
      filterIdentifier item $ bestRelated needle tags


filterIdentifier :: Item a -> [Identifier] -> [Identifier]
filterIdentifier item = filter (itemIdentifier item /=)


bestRelated :: [String] -> Tags -> [Identifier]
bestRelated needle tags = (sortByFrequency $ selectTags needle tags) ++ (nub $ concatMap snd $ tagsMap tags)


tagsByItem :: Item a -> Compiler [String]
tagsByItem = getTags . itemIdentifier


toPatternWith :: Version -> [Identifier] -> Pattern
toPatternWith version = fromList . map (setVersion $ Just version)


selectTags :: [String] -> Tags -> [Identifier]
selectTags needle tags = concatMap snd $ onlyNeeded (tagsMap tags)
  where
    onlyNeeded :: [(String, [Identifier])] -> [(String, [Identifier])]
    onlyNeeded = filter (\i -> fst i `elem` needle)


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

