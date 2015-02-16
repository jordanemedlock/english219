 {-# LANGUAGE OverloadedStrings #-}
module Handler.Project3 where

import Import

getProject3R :: Handler Html
getProject3R = do
    let route = Project3R
    defaultLayout $ do
        setTitle "Jordan E Medlock - Project 3"
        $(widgetFile "menu")
        [whamlet|Project 3|]
