module Handler.Article where

import Import
import Text.Markdown
import Yesod.Form.Functions
import Yesod.Form.Fields
import Yesod.Form.Bootstrap3

getArticleR :: ArticleId -> Handler Html
getArticleR articleId = do
    article <- runDB $ get404 articleId
    defaultLayout $ do
        setTitle $ toHtml $ articleTitle article
        $(widgetFile "article")

deleteArticleR :: ArticleId -> Handler Html
deleteArticleR articleId = do
    runDB $ delete articleId
    redirect BlogR

getDeleteArticleR :: ArticleId -> Handler Html
getDeleteArticleR articleId = do
    runDB $ delete articleId
    redirect BlogR

deleteDeleteArticleR :: ArticleId -> Handler Html
deleteDeleteArticleR articleId = do
    runDB $ delete articleId
    redirect BlogR

getEditArticleR :: ArticleId -> Handler Html
getEditArticleR articleId = do
    article <- runDB $ get404 articleId
    (articleWidget, enctype) <- generateFormPost entryForm
    defaultLayout $ do

        $(widgetFile "edit-article")

postEditArticleR :: ArticleId -> Handler Html
postEditArticleR articleId = do
    ((res,articleWidget),enctype) <- runFormPost entryForm
    case res of
        FormSuccess article -> do
            runDB $ updateWhere [ArticleId ==. articleId] [ArticleTitle =. (articleTitle article), ArticleContent =. (articleContent article)]
            setMessage $ toHtml $ (articleTitle article) <> " edited"
            redirect $ ArticleR articleId
        _ -> defaultLayout $ do
                setTitle "Please correct your entry form"
                $(widgetFile "articleAddError")


entryForm :: Form Article
entryForm = renderBootstrap3 BootstrapBasicForm $ (\x y -> Article x (unTextarea y))
    <$> areq   textField "Title" Nothing
    <*> areq   textareaField "Content" Nothing
