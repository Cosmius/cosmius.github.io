{-# LANGUAGE OverloadedStrings #-}
import           Hakyll


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
  let stripSrcRoute = gsubRoute "src/" (const "")
  let directorizeRoute = gsubRoute ".html" (const "/index.html")

  match "templates/*" $ compile templateCompiler

  match "src/index.html" $ do
    route $ stripSrcRoute
    compile $ do
      posts <- loadAll "src/posts/*" >>= recentFirst
      let ctx = listField "posts" postCtx (return posts)
             <> constField "title" "Home"
             <> defaultContext
      getResourceBody
        >>= applyAsTemplate ctx
        >>= loadAndApplyTemplate "templates/layout.html" ctx
        >>= relativizeUrls

  create ["archives.html"] $ do
    route $ directorizeRoute
    compile $ do
      posts <- loadAll "src/posts/*" >>= recentFirst
      let ctx = listField "posts" postCtx (return posts)
             <> constField "title" "Archives"
             <> defaultContext
      makeItem ""
        >>= loadAndApplyTemplate "templates/archives.html" ctx
        >>= loadAndApplyTemplate "templates/layout.html" ctx
        >>= relativizeUrls

  match "src/pages.html" $ do
    route $ stripSrcRoute `composeRoutes` directorizeRoute
    compile $ getResourceBody
          >>= applyAsTemplate defaultContext
          >>= loadAndApplyTemplate "templates/layout.html" defaultContext
          >>= relativizeUrls

  match "src/tkhe.html" $ do
    route $ stripSrcRoute `composeRoutes` directorizeRoute
    compile $ getResourceBody
          >>= applyAsTemplate defaultContext
          >>= loadAndApplyTemplate "templates/layout.html" defaultContext
          >>= relativizeUrls

  match "src/posts/*" $ do
    route $ stripSrcRoute `composeRoutes` setExtension "html"
    compile $ pandocCompiler
          >>= loadAndApplyTemplate "templates/post.html"    postCtx
          >>= loadAndApplyTemplate "templates/layout.html" postCtx
          >>= relativizeUrls

  match "src/css/*" $ do
    route   stripSrcRoute
    compile compressCssCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext
