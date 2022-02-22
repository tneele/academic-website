{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Handler.Other where

import Import

getResearchR :: Handler Html
getResearchR = defaultLayout $ do
    setPageName "Research"
    $(widgetFile "research")

getStudentsR :: Handler Html
getStudentsR = defaultLayout $ do
    setPageName "Students"
    $(widgetFile "students")

getCoursesR :: Handler Html
getCoursesR = defaultLayout $ do
    setPageName "Courses"
    $(widgetFile "courses")

getContactR :: Handler Html
getContactR = defaultLayout $ do
    setPageName "Contact"
    $(widgetFile "contact")
