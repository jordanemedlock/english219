 {-# LANGUAGE OverloadedStrings #-}
module Handler.Project2 where

import Import

getProject2R :: Handler Html
getProject2R = do
    let route = Project2R
    defaultLayout $ do
        setTitle "Jordan E Medlock - Project 2"
        $(widgetFile "menu")
        $(widgetFile "project-2")
