{-# LANGUAGE TemplateHaskell #-}

module Handler.Home where

import Import
import Prelude ((!!))
import System.Random

getRandomElement :: [a] -> IO a
getRandomElement xs = do
    i <- getStdRandom (randomR (0,(length xs) - 1))
    return $ xs !! i

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = do
    headerRoute <- liftIO $ getRandomElement $ map StaticR [img_header_1_jpg, img_header_2_jpg, img_header_3_jpg, img_header_4_jpg]
    defaultLayout $ do
        app <- getYesod
        setTitle $ toHtml $ appSiteName $ appSettings app
        $(widgetFile "homepage")
