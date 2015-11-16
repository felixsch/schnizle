module Schnizle.Config
  ( config
  , pandocOptions
  ) where

import Hakyll

import qualified Text.Pandoc.Options as O
import Text.Highlighting.Kate.Styles (pygments)

config :: Configuration
config = defaultConfiguration
  { deployCommand = "rsync -avz -e ssh ./_site/ schnizle.in:/home/felixsch/html/" }

pandocOptions :: O.WriterOptions
pandocOptions = defaultHakyllWriterOptions
    { O.writerHtml5 = True
    , O.writerHtmlQTags = True
    , O.writerSectionDivs = True
    , O.writerTableOfContents = True
    , O.writerHighlight = True
    , O.writerHighlightStyle = pygments
    , O.writerExtensions = O.githubMarkdownExtensions
    }
