module Handler.LearnHaskell where

import Import
import Prelude ((!!))


getLearnHaskellR :: Int -> Handler Html
getLearnHaskellR page = do
    let route = LearnTOCR
        partialRoute = LearnHaskellR
        pageNumbers = [1..6] :: [Int]
        pageTitles = map toHtml (["Introduction","Install Haskell","Learn Haskell","Solve Problem 1","Solve Problem 2","Solve Problem 3","Conclusion"] :: [String])
    defaultLayout $ do
        setTitle "Jordan E Medlock - Learn Haskell"
        $(widgetFile "menu")
        $(widgetFile "paginator-top")
        case page of 
              1 -> $(widgetFile "haskell-intro")
              2 -> $(widgetFile "haskell-install")
              3 -> $(widgetFile "haskell-learn")
              4 -> $(widgetFile "haskell-problem1")
              5 -> $(widgetFile "haskell-problem2")
              6 -> $(widgetFile "haskell-problem3")
              -- 7 -> $(widgetFile "haskell-conclusion")
              _ -> notFound
        $(widgetFile "paginator-top")