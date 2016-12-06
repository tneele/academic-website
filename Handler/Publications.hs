module Handler.Publications where

import Import
import Yesod.Form.Bootstrap3 (renderBootstrap3, bfs, BootstrapFormLayout(BootstrapBasicForm))
import Network.Wai (requestMethod)
--import Network.HTTP.Types.Method (methodGet,methodPost)

getPublicationsR :: Handler Html
getPublicationsR = do
    publications <- runDB $ selectList [] [ Desc PublicationPublished ]
    maid <- maybeAuthId
    defaultLayout $ do
        setTitle "Publications"
        $(widgetFile "publications")
    where
        fst3 (a,_,_) = a

publicationForm :: Maybe Publication -> AForm Handler Publication
publicationForm mpublication = Publication
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

handleForm :: (RedirectUrl (HandlerSite Handler) url) => Text -> (Widget -> Enctype -> Widget) -> Maybe a -> (Maybe a -> AForm Handler a) -> (a -> Handler b) -> url -> Handler Html
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

handleEditPublicationR :: PublicationId -> Handler Html
handleEditPublicationR publicationId = do
    publication <- runDB $ get404 publicationId
    handleForm "publication" (\widget enctype -> $(widgetFile "publication-edit")) (Just publication) publicationForm (\pub -> runDB $ replace publicationId pub) PublicationsR

handleAddPublicationR :: Handler Html
handleAddPublicationR =
    handleForm "publication" (\widget enctype -> $(widgetFile "publication-add")) Nothing publicationForm (\pub -> runDB $ insert pub) PublicationsR
