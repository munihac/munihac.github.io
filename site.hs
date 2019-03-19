{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative
import           Data.Aeson.Types
import qualified Data.HashMap.Strict as M
import           Data.Maybe
import           Data.Monoid         ((<>))
import           Data.Scientific
import qualified Data.Text           as T
import           Debug.Trace
import           Hakyll

main :: IO ()
main = hakyll $ do

    match "favicon.ico" $ do
        route   idRoute
        compile copyFileCompiler

    match "img/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "img/**/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "js/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "fonts/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "templates/*" $ compile templateBodyCompiler

    match "templates/**/*" $ compile templateBodyCompiler

    match "hotels/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= relativizeUrls

    create ["index.html"] $ do
        route idRoute
        compile $ makeItem $ Redirect "2018.html"

    match "2018.html" compileMainPage

    match "impressum.html" $ do
        route idRoute
        compile $
            getResourceBody
                >>= applyAsTemplate defaultContext
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls

compileMainPage :: Rules ()
compileMainPage = do
    route idRoute
    compile $ do
        hotels <- loadAll "hotels/*"
        let indexCtx =
                hotelCtx hotels
                <> constField "title" "MuniHac 2018"
                <> defaultContext

        getResourceBody
            >>= applyAsTemplate indexCtx
            >>= loadAndApplyTemplate "templates/default.html" indexCtx
            >>= relativizeUrls

hotelCtx :: [Item String] -> Context String
hotelCtx hotels = listField "hotels"
                     (  field "name" (extractMetaData "name")
                     <> field "url" (extractMetaData "url")
                     <> listFieldWith "stars"
                            (field "class" (return . itemBody))
                            (\item -> do
                                k <- extractMetaData' "stars" item
                                mapM makeItem [if i <= k then "glyphicon-star" else "glyphicon-star-empty" | i <- [1..5]])
                     <> field "street" (extractMetaData "street")
                     <> field "zip" (extractMetaData "zip")
                     <> field "city" (extractMetaData "city")
                     <> field "remark" (\item -> getMetadataField (itemIdentifier item) "remark" >>= maybe empty return)
                     <> field "mapsurl" (extractMetaData "mapsurl")
                    )
                    (return hotels)
               <> defaultContext

extractMetaData :: MonadMetadata m => String -> Item a -> m String
extractMetaData name item = getMetadataField' (itemIdentifier item) name


extractMetaData' :: MonadMetadata m => T.Text -> Item a -> m Int
extractMetaData' name item = do
    let identifier = itemIdentifier item
    metadata <- getMetadata identifier
    let result =
          case M.lookup name metadata of
            Nothing    -> error $ "Item " ++ show identifier ++ " has no metadata field " ++ show name
            Just value ->
              case value of
                Number n -> fromJust (toBoundedInteger n)
                other    -> error $ "Item " ++ show identifier ++ " is of unknown type: " ++ show other
    return result
