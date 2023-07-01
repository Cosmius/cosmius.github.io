{-# LANGUAGE OverloadedStrings #-}
import           Hakyll


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
  match "templates/*" $ compile templateCompiler

  match "src/index.html" $ do
    route $ constRoute "index.html"
    compile $ getResourceBody
          >>= applyAsTemplate defaultContext
          >>= loadAndApplyTemplate "templates/layout.html" defaultContext
          >>= relativizeUrls

  match "src/tkhe.html" $ do
    route $ constRoute "tkhe/index.html"
    compile $ getResourceBody
          >>= applyAsTemplate defaultContext
          >>= loadAndApplyTemplate "templates/layout.html" defaultContext
          >>= relativizeUrls


--------------------------------------------------------------------------------
