{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
import qualified GHC.IO.Encoding as E

import           Data.Monoid         (mappend)

import           Hakyll
import qualified Text.Pandoc.Options as P.O


main :: IO ()
main = do
  E.setLocaleEncoding E.utf8
  hakyll $ do
    let
      readerOptions
        = defaultHakyllReaderOptions
      writerOptions
        = defaultHakyllWriterOptions
            { P.O.writerHTMLMathMethod = P.O.MathJax ""
            }
      compiler
        = pandocCompilerWith readerOptions writerOptions

    tags <- buildTags "posts/*" (fromCapture "tags/*.html")

    match "images/*" $ do
      route   idRoute
      compile copyFileCompiler

    match "css/*" $ do
      route   idRoute
      compile compressCssCompiler

    match (fromList ["about.md", "talks.md"]) $ do
      route   $ setExtension "html"
      compile $ pandocCompiler
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls

    match "posts/*" $ do
      route $ setExtension "html"
      compile $ compiler
        >>= loadAndApplyTemplate "templates/post.html"  (postCtxWithTags tags)
        >>= loadAndApplyTemplate "templates/default.html" (postCtxWithTags tags)
        >>= relativizeUrls

    create ["archive.html"] $ do
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll "posts/*"
        let
          archiveCtx
            =  listField "posts" (postCtxWithTags tags) (return posts)
            <> constField "title" "Posts"
            <> defaultContext

        makeItem ""
          >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
          >>= loadAndApplyTemplate "templates/default.html" archiveCtx
          >>= relativizeUrls


    match "index.html" $ do
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll "posts/*"
        let
          indexCtx
            =  listField "posts" (postCtxWithTags tags) (return posts)
            <> constField "title" "Posts"
            <> defaultContext

        getResourceBody
          >>= applyAsTemplate indexCtx
          >>= loadAndApplyTemplate "templates/default.html" indexCtx
          >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler

    tagsRules tags $ \tag pat -> do
      let title = "Posts tagged \"" ++ tag ++ "\""
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll pat
        let
          ctx
            =  constField "title" title
            <> listField "posts" (postCtxWithTags tags) (return posts)
            <> defaultContext

        makeItem ""
            >>= loadAndApplyTemplate "templates/tag.html" ctx
            >>= loadAndApplyTemplate "templates/default.html" ctx
            >>= relativizeUrls

    create ["tags.html"] $ do
      route idRoute
      let tagList = tagsMap tags
      compile $ do
        makeItem ""
          >>= loadAndApplyTemplate "templates/tag-list.html" (defaultCtxWithTags tags)
          >>= loadAndApplyTemplate "templates/default.html" (constField "title" "Tags" <> defaultCtxWithTags tags)
          >>= relativizeUrls

defaultCtxWithTags :: Tags -> Context String
defaultCtxWithTags tags = listField "tags" tagsCtx getAllTags         `mappend`
                          defaultContext
  where getAllTags :: Compiler [Item (String, [Identifier])]
        getAllTags = mapM (pure . mkItem) $ tagsMap tags
          where mkItem :: (String, [Identifier]) -> Item (String, [Identifier])
                mkItem x@(t, _) = Item (tagsMakeId tags t) x
        tagsCtx :: Context (String, [Identifier])
        tagsCtx = listFieldWith "posts" postsCtx getPosts             `mappend`
                  metadataField                                       `mappend`
                  urlField "url"                                      `mappend`
                  pathField "path"                                    `mappend`
                  titleField "title"                                  `mappend`
                  missingField
          where getPosts :: Item (String, [Identifier])
                         -> Compiler [Item String]
                getPosts (itemBody -> (_, is)) = mapM load is
                postsCtx :: Context String
                postsCtx = postCtxWithTags tags

postCtxWithTags :: Tags -> Context String
postCtxWithTags tags
  =  tagsField "tags" tags
  <> postCtx

postCtx :: Context String
postCtx
  =  dateField "date" "%B %e, %Y"
  <> dateField "shortDate" "%Y.%m.%d"
  <> defaultContext
