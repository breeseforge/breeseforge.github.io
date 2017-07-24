--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import Control.Monad.IO.Class
import Data.Monoid (mappend)
import Hakyll


--------------------------------------------------------------------------------
main :: IO ()
main = do
        galleryImages <- (++) <$> (fmap ("images/knives/"++) <$> getRecursiveContents (\_ -> return False) "images/knives")
                             <*> (fmap ("images/ironwork/"++) <$> getRecursiveContents (\_ -> return False) "images/ironwork")
        print galleryImages
        hakyll $ do
            match "images/*/*" $ do
                route   idRoute
                compile copyFileCompiler
            match "css/*" $ do
                route   idRoute
                compile compressCssCompiler
            match (fromList ["about.rst", "contact.markdown"]) $ do
                route   $ setExtension "html"
                compile $ pandocCompiler
                    >>= loadAndApplyTemplate "templates/default.html" defaultContext
                    >>= relativizeUrls
            match "knives/*" $ do
                route $ setExtension "html"
                compile $ pandocCompiler
                    >>= loadAndApplyTemplate "templates/knife.html" defaultContext
                    >>= loadAndApplyTemplate "templates/default.html" defaultContext
                    >>= relativizeUrls
            create ["gallery.html"] $ do
                route idRoute
                compile $ do
                    let archiveCtx =
                            listField "items" imgCtx (return $ (\p -> Item (fromFilePath p) p) <$> galleryImages) `mappend`
                            constField "title" "Gallery" `mappend`
                            defaultContext
                    makeItem ""
                        >>= loadAndApplyTemplate "templates/gallery.html" archiveCtx
                        >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                        >>= relativizeUrls
            create ["forsale_knives.html"] $ do
                route idRoute
                compile $ do
                    items <- recentFirst =<< loadAll "knives/*"
                    let archiveCtx =
                            listField "items" defaultContext (return items) `mappend`
                            constField "title" "Knives For Sale" `mappend`
                            defaultContext
                    makeItem ""
                        >>= loadAndApplyTemplate "templates/forsale.html" archiveCtx
                        >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                        >>= relativizeUrls
            create ["forsale_ironwork.html"] $ do
                route idRoute
                compile $ do
                    items <- recentFirst =<< loadAll "ironwork/*"
                    let archiveCtx =
                            listField "items" defaultContext (return items) `mappend`
                            constField "title" "Ironwork For Sale" `mappend`
                            defaultContext
                    makeItem ""
                        >>= loadAndApplyTemplate "templates/gallery.html" archiveCtx
                        >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                        >>= relativizeUrls
            match "forsale.html" $ do
                route idRoute
                compile $ do
                    let forsaleCtx = constField "title" "For Sale" `mappend` defaultContext
                    getResourceBody
                        >>= applyAsTemplate forsaleCtx
                        >>= loadAndApplyTemplate "templates/default.html" forsaleCtx
                        >>= relativizeUrls
            match "index.html" $ do
                route idRoute
                compile $ do
                    knives <- recentFirst =<< loadAll "knives/*"
                    let indexCtx =
                            listField "knives" defaultContext (return knives) `mappend`
                            constField "title" "Home" `mappend`
                            defaultContext
                    getResourceBody
                        >>= applyAsTemplate indexCtx
                        >>= loadAndApplyTemplate "templates/default.html" indexCtx
                        >>= relativizeUrls
            match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------

imgCtx :: Context String
imgCtx = field "url" $ \item -> return (itemBody item)
