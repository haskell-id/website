-------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Main where
import           Hakyll
import           Text.Highlighting.Kate (pygments, styleToCss)
-------------------------------------------------------------------------------


toHTML :: Context String -> Rules ()
toHTML c =  do
  route $ setExtension "html"
  compile $ pandocCompiler
    >>= loadAndApplyTemplate "templates/body.html" c
    >>= loadAndApplyTemplate "templates/base.html" c
    >>= relativizeUrls



-- static
static :: Rules ()
static = match "static/**" $ do
  route   idRoute
  compile copyFileCompiler


-- kompres berkas2 css
cssCompressor :: Rules ()
cssCompressor = match "static/css/*" $ do
  route   idRoute
  compile compressCssCompiler


-- generate css untuk sintaks highlight
cssHighlight :: Rules ()
cssHighlight = create ["static/css/style.css"] $ do
  route idRoute
  compile $ makeItem (compressCss . styleToCss $ pygments)


-- top level pages, *.md compiled to *.html
topPages :: Rules ()
topPages = match ("*.md" .&&. complement "README.md") $ do
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


templates :: Rules ()
templates = match "templates/*" $ compile templateCompiler


siteIndex :: Rules ()
siteIndex = match "index.html" $ do
  route idRoute
  compile $ do
    getResourceBody
      >>= loadAndApplyTemplate "templates/base.html" defaultContext
      >>= relativizeUrls


cname :: Rules ()
cname = match "CNAME" $ do
  route idRoute
  compile copyFileCompiler


main :: IO ()
main = hakyll $ do
  cssCompressor
  cssHighlight
  static
  topPages
  lectureHtml
  lectureRaw
  templates
  siteIndex
  cname

