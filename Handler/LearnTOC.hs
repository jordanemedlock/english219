module Handler.LearnTOC where

import Import

getLearnTOCR :: Handler Html
getLearnTOCR = do
    let route = LearnTOCR
    defaultLayout $ do
        setTitle "Jordan E Medlock - Learn Haskell Table of Contents"
        $(widgetFile "menu")
        $(widgetFile "haskell-toc")

