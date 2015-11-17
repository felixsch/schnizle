module Schnizle.Fields
  ( relatedPostsField
  ) where

import Data.List
import Data.Typeable
import Data.Binary 
import qualified Data.Map as M

import Hakyll


relatedPostsField :: (Typeable a, Binary a) => String -> Int -> Tags -> Context a -> Context b
relatedPostsField name n tags ctx = listFieldWith name ctx $ \item -> do
  needle <- getTags $ itemIdentifier item
  mapM load $ take n $ sortByFrequency $ selectTags needle tags

selectTags :: [String] -> Tags -> [Identifier]
selectTags needle tags = concatMap snd $ onlyNeeded (tagsMap tags)
  where
    onlyNeeded :: [(String, [Identifier])] -> [(String, [Identifier])]
    onlyNeeded = filter (\i -> (fst i) `elem` needle)

sortByFrequency :: [Identifier] -> [Identifier]
sortByFrequency ids = map snd $ sortBy (\a b -> compare (fst a) (fst b)) $
  map (\l -> (length l, head l)) (group (sort ids))


{-
sortMatchedTags :: Tags -> [String] -> [Identifier]
sortMatchedTags tags needle = map (fromFilePath . fst) $ 
  sortBy cmp $ 
    tagsMap tags
  where
    cmp :: (String, [Identifier]) -> (String, [Identifier]) -> Ordering
    cmp a b         = compare (matched a) (matched b)

    matched :: (String, [Identifier]) -> Int
    matched (_,x)   = sum $ map (\i -> asInteger $ i `elem` needle) x

    asInteger :: Bool -> Int
    asInteger True  = 0
    asInteger False = 1


getAdditionalLinks :: MonadMetadata m => Identifier -> m [String]
getAdditionalLinks identifier = do
    metadata <- getMetadata identifier
    return $ maybe [] (map trim . splitAll "|") $ M.lookup "links" metadata

-}


