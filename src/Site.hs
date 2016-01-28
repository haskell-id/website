-------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Main where
import           Hakyll
import           Text.Highlighting.Kate (pygments, styleToCss)
-------------------------------------------------------------------------------

-- SITE CONTENT
toHTML :: Context String -> Rules ()
toHTML c =  do
  route $ setExtension "html"
  compile $ pandocCompiler
    >>= loadAndApplyTemplate "templates/body.html" c
    >>= loadAndApplyTemplate "templates/base.html" c
    >>= relativizeUrls


-- top level pages, *.md compiled to *.html
topPages :: Rules ()
topPages = match ("*.md") $ do
  toHTML defaultContext

-- lecture materials, html version
lectureHtml :: Rules ()
lectureHtml = match "lectures/*" $ do
  toHTML defaultContext

-- lecture materials, raw lhs files
lectureRaw :: Rules ()
lectureRaw = match "lectures/*" $ version "raw" $ do
  route   idRoute
  compile getResourceBody


siteIndex :: Rules ()
siteIndex = match "index.html" $ do
  route idRoute
  compile $ do
    getResourceBody
      >>= loadAndApplyTemplate "templates/base.html" defaultContext
      >>= relativizeUrls
-- END OF CONTENT


templates :: Rules ()
templates = match "templates/*" $ compile templateCompiler

cname :: Rules ()
cname = match "CNAME" $ do
  route idRoute
  compile copyFileCompiler


-- static
static :: Rules ()
static = match "static/**" $ do
  route   idRoute
  compile copyFileCompiler

-- generate css untuk sintaks highlight
cssHighlight :: Rules ()
cssHighlight = create ["static/css/style.css"] $ do
  route idRoute
  compile $ makeItem (compressCss . styleToCss $ pygments)


-- kompres berkas2 css
cssCompressor :: Rules ()
cssCompressor = match "static/css/*" $ do
  route   idRoute
  compile compressCssCompiler


hakyllConf :: Configuration
hakyllConf = defaultConfiguration {
      deployCommand = "rsync -arvzc _site/ ../haskell-id.github.io/"
   ,  providerDirectory = "provider"
   }

main :: IO ()
main = hakyllWith hakyllConf $ do
  cssCompressor
  cssHighlight
  static
  topPages
  lectureHtml
  lectureRaw
  templates
  siteIndex
  cname

