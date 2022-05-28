{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative
import           Data.Aeson.Types
import qualified Data.Aeson.Key      as K
import qualified Data.Aeson.KeyMap   as M
import           Data.Foldable
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

    match "pages/*" $ compile getResourceBody
    match "pages/**/*" $ compile getResourceBody

    match "hotels/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= relativizeUrls

    create ["index.html"] $ do
        route idRoute
        compile $ makeItem $ Redirect "2020.html"

    createMainPage "2016.html"
        [ "pages/2016/logo.html"
        , "pages/2016/introduction.html"
        , "pages/2016/pictures.html"
        , "pages/2016/keynotes.html"
        , "pages/contact.html"
        , "pages/archive.html"
        ]

    createMainPage "2018.html"
        [ "pages/2018/logo.html"
        , "pages/2018/introduction.html"
        , "pages/2018/pictures.html"
        , "pages/2018/keynotes.html"
        , "pages/2018/projects.html"
        , "pages/2018/schedule.html"
        , "pages/contact.html"
        , "pages/archive.html"
        ]

    createMainPage "2019.html"
        [ "pages/2019/logo.html"
        , "pages/2019/introduction.html"
        , "pages/2019/pictures.html"
        , "pages/2019/keynotes.html"
        , "pages/2019/projects.html"
        , "pages/contact.html"
        , "pages/archive.html"
        ]

    createMainPage "2020.html"
        [ "pages/2020/logo.html"
        , "pages/2020/introduction.html"
        , "pages/2019/pictures.html"
        , "pages/2020/speakers.html"
        , "pages/contact.html"
        , "pages/archive.html"
        ]

    --match "impressum.html" $ do
    --    route idRoute
    --    compile $
    --        getResourceBody
    --            >>= applyAsTemplate defaultContext
    --            >>= loadAndApplyTemplate "templates/default.html" (defaultContext <> snippetField)
    --            >>= relativizeUrls

createMainPage :: Identifier -> [Identifier] -> Rules ()
createMainPage identifier pages = do
    let pagesCtx = listField "pages" defaultContext (traverse load pages)
        menuCtx = listField "menu" defaultContext (traverse load pages)
    let indexCtx = defaultContext <> snippetField <> menuCtx
    for_ pages $ \page -> getMetadataField page "url" >>= \case
        Nothing -> pure ()
        Just url -> create [fromFilePath url] $ do
            route idRoute
            compile $ makeItem ""
                >>= withItemBody (\_ -> loadBody page)
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls
    create [identifier] $ do
        route idRoute
        compile $ makeItem ""
            >>= loadAndApplyTemplate "templates/default.html" (indexCtx <> pagesCtx)
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

extractMetaData :: (MonadMetadata m, MonadFail m) => String -> Item a -> m String
extractMetaData name item = getMetadataField' (itemIdentifier item) name


extractMetaData' :: MonadMetadata m => T.Text -> Item a -> m Int
extractMetaData' name item = do
    let identifier = itemIdentifier item
    metadata <- getMetadata identifier
    let result =
          case M.lookup (K.fromText name) metadata of
            Nothing    -> error $ "Item " ++ show identifier ++ " has no metadata field " ++ show name
            Just value ->
              case value of
                Number n -> fromJust (toBoundedInteger n)
                other    -> error $ "Item " ++ show identifier ++ " is of unknown type: " ++ show other
    return result
