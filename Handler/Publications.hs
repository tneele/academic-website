module Handler.Publications where

import Import
import Yesod.Form.Bootstrap3 (renderBootstrap3, bfs, BootstrapFormLayout(BootstrapBasicForm))

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

getEditPublicationR :: PublicationId -> Handler Html
getEditPublicationR publicationId = do
    publication <- runDB $ get404 publicationId
    (widget, enctype) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm (publicationForm $ Just publication)
    defaultLayout $ do
        setTitle "Edit publication"
        $(widgetFile "publication-edit")

postEditPublicationR :: PublicationId -> Handler Html
postEditPublicationR publicationId = do
    publication <- runDB $ get404 publicationId
    ((result, widget), enctype) <- runFormPost $ renderBootstrap3 BootstrapBasicForm (publicationForm $ Just publication)
    case result of
        FormSuccess pub -> do
            _ <- runDB $ replace publicationId pub
            setMessage $ toHtml $ "Publication " ++ (publicationTitle pub) ++ " was edited succesfully."
            redirect PublicationsR
        FormFailure errors -> do
            setMessage $ toHtml $ "The following errors occured" ++ foldr (++) "" errors
            defaultLayout $ do
                setTitle "Edit publication"
                $(widgetFile "publication-edit")
        FormMissing -> do
            setMessage $ toHtml ("No data was submitted" :: Text)
            defaultLayout $ do
                setTitle "Edit publication"
                $(widgetFile "publication-edit")

getAddPublicationR :: Handler Html
getAddPublicationR = do
    (widget, enctype) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm (publicationForm Nothing)
    defaultLayout $ do
        setTitle "Edit publication"
        $(widgetFile "publication-add")

postAddPublicationR :: Handler Html
postAddPublicationR = do
    ((result, widget), enctype) <- runFormPost $ renderBootstrap3 BootstrapBasicForm (publicationForm Nothing)
    case result of
        FormSuccess pub -> do
            _ <- runDB $ insert pub
            setMessage $ toHtml $ "Publication " ++ (publicationTitle pub) ++ " was added succesfully."
            redirect PublicationsR
        FormFailure errors -> do
            setMessage $ toHtml $ "The following errors occured" ++ foldr (++) "" errors
            defaultLayout $ do
                setTitle "Edit publication"
                $(widgetFile "publication-add")
        FormMissing -> do
            setMessage $ toHtml ("No data was submitted" :: Text)
            defaultLayout $ do
                setTitle "Edit publication"
                $(widgetFile "publication-add")