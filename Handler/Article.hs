module Handler.Article where

import Import
import Text.Markdown

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