-------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Main where
import           Hakyll
import           Text.Highlighting.Kate (pygments, styleToCss)
-------------------------------------------------------------------------------

main :: IO ()
main = hakyll $ do

    match "static/css/*" $ do
        route   idRoute
        compile compressCssCompiler

    create ["static/css/style.css"] $ do
        route idRoute
        compile $ makeItem (compressCss . styleToCss $ pygments)

    match "static/images/**" $ do
        route   idRoute
        compile copyFileCompiler

    -- lecture index, compiled to /lectures.html
    match "lectures/lectures.md" $ do
        route $ gsubRoute "lectures/" (const "") `composeRoutes` setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/body.html" defaultContext
            >>= loadAndApplyTemplate "templates/base.html" defaultContext
            >>= relativizeUrls

    -- lecture materials, html version
    match "lectures/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/body.html" defaultContext
            >>= loadAndApplyTemplate "templates/base.html" defaultContext
            >>= relativizeUrls

    -- lecture materials, raw lhs files
    match "lectures/*" $ version "raw" $ do
        route   idRoute
        compile getResourceBody

    match "index.html" $ do
         route idRoute
         compile $ do
             getResourceBody
                 >>= loadAndApplyTemplate "templates/base.html" defaultContext
                 >>= relativizeUrls

    match "templates/*" $ compile templateCompiler

