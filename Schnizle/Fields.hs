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

import Control.Monad


-- related links -------------------------------------------------------------
type Score = Int

data Related = Related
  { relatedId    :: Identifier
  , relatedUrl   :: String
  , relatedDate  :: String
  , relatedTitle :: String }

type RelatedLinks = [Related]

buildRelatedLinksWith :: MonadMetaData m => Pattern -> (Related -> Int) -> m RelatedLinks
buildRelatedLinksWith pattern rate = do
  matches <- getMatches pattern
  related <- mapM genRelated matches
  return $ sortByScore $ map (\r -> ())

  
genRelated :: MonadMetaData m => Item a -> m Related
genRelated item = do
  where 
    ident = return $ itemIdentifier item
    url   = maybe empty toUrl <$> getRoute $ itemIdentifier item
    date  = 
    
    

  

relatedPostsField :: (Typeable a, Binary a) => String -> Version -> Tags -> Context a -> Context b
relatedPostsField name version tags ctx = listFieldWith name ctx $ \item -> do
  needle <- tagsByItem item 
  unsafeCompiler $ putStrLn " ----------- "
  unsafeCompiler $ putStrLn $ " -- item:   " ++ show (itemIdentifier item)
  unsafeCompiler $ putStrLn $ " -- needle: " ++ show needle
  unsafeCompiler $ putStrLn $ "\n -- bestRelated: "
  unsafeCompiler $ forM_ (bestRelated needle tags) print
  unsafeCompiler $ putStrLn $ "\n -- tag suggestion: "
  unsafeCompiler $ forM_ (selectTags needle tags) print
  unsafeCompiler $ putStrLn $ "\n -- related: "
  unsafeCompiler $ forM_ (related item needle) print
  unsafeCompiler $ putStrLn $ "\n\n\n"
  loadAll $ toPatternWith version (related item needle)

  where
    related item needle = take 2 $ 
      filterIdentifier item $ bestRelated needle tags


filterIdentifier :: Item a -> [Identifier] -> [Identifier]
filterIdentifier item = filter (itemIdentifier item /=)


bestRelated :: [String] -> Tags -> [Identifier]
bestRelated needle tags = nub $ (sortByFrequency $ selectTags needle tags) ++ (concatMap snd $ tagsMap tags)


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

