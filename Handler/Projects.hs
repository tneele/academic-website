{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Handler.Projects where

import Import

getProjectsR :: Handler Html
getProjectsR = defaultLayout $ do
    setPageName "Projects"
    $(widgetFile "projects")
