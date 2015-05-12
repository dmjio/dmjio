{-# LANGUAGE RecordWildCards #-}
--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid
import Data.Monoid (mappend)
import Hakyll

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    tags <- buildTags "posts/*" (fromCapture "tags/*.html")

    tagsRules tags $ \tag pattern -> do
      let title = "Posts tagged \"" ++ tag ++ "\""
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll pattern
        let ctx = constField "title" title
                  `mappend` listField "posts" (postCtxWithTeaserAndTags tags) (return posts)
                  `mappend` defaultContext
        makeItem "" >>= loadAndApplyTemplate "templates/tag.html" ctx
                    >>= loadAndApplyTemplate "templates/default.html" ctx
                    >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    (postCtxWithTeaserAndTags tags)
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/default.html" (postCtxWithTeaserAndTags tags)
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" (postCtxWithTeaserAndTags tags) (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- take 10 <$> do recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" (postCtxWithTeaserAndTags tags) (return posts) `mappend`
                    field "tags" (\_ -> renderTagCloud 100 200 tags) `mappend`
                    constField "title" "Home"                `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler

    create ["atom.xml"] $ do
      route idRoute
      compile $ do
        let feedCtx = postCtx `mappend` bodyField "description"
        posts <- fmap (take 10) . recentFirst =<<
          loadAllSnapshots "posts/*" "content"
        renderAtom myFeedConfiguration feedCtx posts


postCtxWithTags :: Tags -> Context String
postCtxWithTags tags = tagsField "tags" tags `mappend`
                       postCtx

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

postCtxWithTeaserAndTags :: Tags -> Context String
postCtxWithTeaserAndTags tags = do
  teaserField "teaser" "content" <> postCtxWithTags tags

------------------------------------------------------------------------------
-- | Feed Conf
myFeedConfiguration :: FeedConfiguration
myFeedConfiguration =
    FeedConfiguration {
      feedTitle       = "dmj.io"
    , feedDescription = "Personal blog of David Johnson"
    , feedAuthorName  = "David Johnson"
    , feedAuthorEmail = "djohnson.m@gmail.com"
    , feedRoot        = "http://dmj.io"
    }

