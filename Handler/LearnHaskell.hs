module Handler.LearnHaskell where

import Import
import Prelude ((!!))


getLearnHaskellR :: Int -> Handler Html
getLearnHaskellR page = do
    let route = LearnHaskellR 1
        partialRoute = LearnHaskellR
        pageNumbers = [1,2,3,4] :: [Int]
        pageTitles = map toHtml (["Introduction","Install Haskell","Learn Haskell","Solve Project Euler"] :: [String])
    defaultLayout $ do
        setTitle "Jordan E Medlock - Learn Haskell"
        $(widgetFile "menu")
        $(widgetFile "paginator-top")
        case page of 
              1 -> $(widgetFile "haskell-intro")
              2 -> $(widgetFile "haskell-install")
              3 -> $(widgetFile "haskell-learn")
              4 -> $(widgetFile "haskell-do")
              5 -> $(widgetFile "haskell-conclusion")
              _ -> notFound
        $(widgetFile "paginator-top")