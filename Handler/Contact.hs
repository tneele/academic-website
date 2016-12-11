module Handler.Contact where

import Import

getContactR :: Handler Html
getContactR = defaultLayout $ do
    setPageName "Contact"
    $(widgetFile "contact")