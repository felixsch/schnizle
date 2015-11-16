module Schnizle.Haml where

import System.IO
import System.Process
import System.Directory

import Hakyll.Core.Compiler
import Hakyll.Core.Item
import Hakyll.Core.Identifier

import Hakyll.Web.Template

hamlCompiler :: Compiler (Item Template)
hamlCompiler = cached "Schnizle.Haml.hamlCompiler" $ do
    item <- getHaml
    return $ fmap readTemplate item

getHaml :: Compiler (Item String)
getHaml = do
  id' <- getUnderlying

  let path = toFilePath id'

  exists <- unsafeCompiler $ doesFileExist path
  if exists
    then unsafeCompiler $ Item id' <$> loadAndRenderHaml path
    else fail $ notfound path
  where
    notfound path = "Schnizle.Haml.getHaml: resource" ++ show path ++ " not found"



loadAndRenderHaml :: FilePath -> IO String
loadAndRenderHaml path = do
  (_, Just hout, _, _) <- createProcess haml
  hGetContents hout  
  where
    haml = (proc "haml" [path]){ std_out = CreatePipe }




