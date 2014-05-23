-------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Main where
import           Hakyll
-------------------------------------------------------------------------------

main :: IO ()
main = hakyll $ do

    match "static/css/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "lectures/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html" defaultContext
            >>= loadAndApplyTemplate "templates/base.html" defaultContext
            >>= relativizeUrls

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

