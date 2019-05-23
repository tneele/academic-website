{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Publications where

import Import
import Yesod.Form.Bootstrap3 (renderBootstrap3, bfs, BootstrapFormLayout(BootstrapBasicForm))
import Network.Wai (requestMethod)
--import Network.HTTP.Types.Method (methodGet,methodPost)

getPublicationsR :: Handler Html
getPublicationsR = do
    publications <- groupBy (\(Entity _ p) (Entity _ q) -> publicationYear p == publicationYear q) `fmap` (runDB $ selectList [] [ Desc PublicationPublished ])
    maid <- maybeAuthId
    defaultLayout $ do
        setPageName "Publications"
        $(widgetFile "publications")
    where
        fst3 (a,_,_) = a
        publicationYear = fst3 . toGregorian . publicationPublished

getPublicationUrl :: Publication -> Maybe Text
getPublicationUrl (Publication _ _ _ _ _ _ _ _ _ _ (Just doi) _) = Just $ "https://doi.org/" ++ doi
getPublicationUrl (Publication _ _ _ _ _ _ _ _ _ (Just url) _ _) = Just url
getPublicationUrl _ = Nothing

data PublicationForm = PublicationForm (Maybe String -> Publication) (Maybe FileInfo)

publicationForm :: Maybe Publication -> AForm Handler PublicationForm
publicationForm mpublication = PublicationForm <$> (Publication
    <$> areq textField (bfs ("Title" :: Text)) (publicationTitle <$> mpublication)
    <*> areq textField (bfs ("Authors" :: Text)) (publicationAuthors <$> mpublication)
    <*> areq dayField  (bfs ("Published" :: Text)) (publicationPublished <$> mpublication)
    <*> aopt textField (bfs ("Publisher" :: Text)) (publicationPublisher <$> mpublication)
    <*> aopt textField (bfs ("Volume title" :: Text)) (publicationVolumeTitle <$> mpublication)
    <*> aopt textField (bfs ("Pages" :: Text)) (publicationPages <$> mpublication)
    <*> aopt textField (bfs ("Series" :: Text)) (publicationSeries <$> mpublication)
    <*> aopt textField (bfs ("Volume" :: Text)) (publicationVolume <$> mpublication)
    <*> aopt textField (bfs ("Issue" :: Text)) (publicationIssue <$> mpublication)
    <*> aopt textField (bfs ("URL" :: Text)) (publicationUrl <$> mpublication)
    <*> aopt textField (bfs ("DOI" :: Text)) (publicationDoi <$> mpublication))
    <*> fileAFormOpt "Paper PDF"

handleForm :: (RedirectUrl (HandlerSite Handler) url) => Text -> (Widget -> Enctype -> Widget) -> Maybe Publication -> (Maybe Publication -> AForm Handler PublicationForm) -> (PublicationForm -> Handler b) -> url -> Handler Html
handleForm obj_name template obj form succ_f red_url = do
    req <- waiRequest
    case parseMethod $ requestMethod req of
        Right GET -> do
            (widget, enctype) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm (form obj)
            defaultLayout $ do
                setTitle $ toHtml $ "Edit " ++ obj_name
                template widget enctype
        Right POST -> do
            ((result, widget), enctype) <- runFormPost $ renderBootstrap3 BootstrapBasicForm (form obj)
            case result of
                FormSuccess form_result -> do
                    _ <- succ_f form_result
                    setMessage $ toHtml $ "Succesfully edited " ++ obj_name ++ "."
                    redirect red_url
                FormFailure errors -> do
                    setMessage $ toHtml $ "The following errors occured" ++ foldr (++) "" errors
                    defaultLayout $ do
                        setTitle $ toHtml $ "Edit " ++ obj_name
                        template widget enctype
                FormMissing -> do
                    setMessage $ toHtml ("No data was submitted" :: Text)
                    defaultLayout $ do
                        setTitle $ toHtml $ "Edit " ++ obj_name
                        template widget enctype
        _ -> badMethod

uploadDirectory :: FilePath
uploadDirectory = "static/papers"

paperFilePath :: String -> FilePath
paperFilePath f = uploadDirectory </> f

writeFileToServer :: FileInfo -> Handler FilePath
writeFileToServer file = do
    let filename = unpack $ fileName file
        path = paperFilePath filename
    liftIO $ fileMove file path
    return filename

addPublication :: PublicationForm -> Handler (Key Publication)
addPublication (PublicationForm incompletePub mfileinfo) = do
    mFileName <- writeFileToServer `mapM` mfileinfo
    let pub = incompletePub mFileName
    runDB $ insert pub

editPublication :: PublicationId -> PublicationForm -> Handler ()
editPublication publicationId (PublicationForm incompletePub mfileinfo) = do
    mFileName <- writeFileToServer `mapM` mfileinfo
    let pub = incompletePub mFileName
    runDB $ replace publicationId pub

handleEditPublicationR :: PublicationId -> Handler Html
handleEditPublicationR publicationId = do
    publication <- runDB $ get404 publicationId
    handleForm "publication" (\widget enctype -> $(widgetFile "publication-edit")) (Just publication) publicationForm (editPublication publicationId) PublicationsR

handleAddPublicationR :: Handler Html
handleAddPublicationR =
    handleForm "publication" (\widget enctype -> $(widgetFile "publication-add")) Nothing publicationForm addPublication PublicationsR
