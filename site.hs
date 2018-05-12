--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid     (mappend)
import           Hakyll
import           System.FilePath
import           Data.List
import           Data.Ord
import           Control.Monad
import           Data.Time
import           Data.Monoid


--------------------------------------------------------------------------------
main :: IO ()
main = hakyllWith customConf $ do

    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "css/**" $ do
        route idRoute
        compile copyFileCompiler

    match "CNAME" $ do
        route $ customRoute (takeFileName . toFilePath)
        compile copyFileCompiler

    match (fromList ["resume.md", "contact.md"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archive"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Home"                `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler

    match "404.html" $ do
      route idRoute
      compile $ pandocCompiler
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls

    create ["feed.xml"] $ do
      route idRoute
      compile $ do
        loadAllSnapshots "posts/*" "content"
          >>= (fmap (take 10)) . createdFirst
          >>= renderAtom feedConfiguration feedCtx
            where
              feedCtx :: Context String
              feedCtx =  defaultContext <>
                          bodyField "description"

--------------------------------------------------------------------------------

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = "Functional programming"
    , feedDescription = "Functional programming ideas"
    , feedAuthorName  = "nagarjuna pamu"
    , feedAuthorEmail = "nagarjuna.pamu@gmail.com"
    , feedRoot        = "https://haskworks.com"
    }

--------------------------------------------------------------------------------

createdFirst :: [Item String] -> Compiler [Item String]
createdFirst items = do
  itemsWithTime <- forM items $ \item -> do
    utc <- getItemUTC defaultTimeLocale $ itemIdentifier item
    return (utc,item)
  return $ map snd $ reverse $ sortBy (comparing fst) itemsWithTime

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

--------------------------------------------------------------------------------
customConf :: Configuration
customConf = defaultConfiguration {
  destinationDirectory = "docs"
}
