-------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Main where
import           Hakyll
import           Text.Highlighting.Kate (pygments, styleToCss)
-------------------------------------------------------------------------------


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


-- lecture index, compiled to /lectures.html
lectureIndex :: Rules ()
lectureIndex = match "lectures.md" $ do
  route $ setExtension "html"
  compile $ pandocCompiler
    >>= loadAndApplyTemplate "templates/body.html" defaultContext
    >>= loadAndApplyTemplate "templates/base.html" defaultContext
    >>= relativizeUrls


-- lecture materials, html version
lectureHtml :: Rules ()
lectureHtml = match "lectures/*" $ do
  route $ setExtension "html"
  compile $ pandocCompiler
    >>= loadAndApplyTemplate "templates/body.html" defaultContext
    >>= loadAndApplyTemplate "templates/base.html" defaultContext
    >>= relativizeUrls


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
  lectureIndex
  lectureHtml
  lectureRaw
  templates
  siteIndex
  cname

