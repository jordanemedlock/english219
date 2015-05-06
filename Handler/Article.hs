module Handler.Article where

import Import
import Yesod.Markdown
import Model.Article

getArticleR :: ArticleId -> Handler Html
getArticleR articleId = do
    article <- runDB $ get404 articleId
    liftIO $ print (articleTimePosted article)
    defaultLayout $ do
        setTitle $ toHtml $ articleTitle article
        let route = BlogR
        $(widgetFile "menu")
        $(widgetFile "article")

deleteArticleR :: ArticleId -> Handler Html
deleteArticleR articleId = do
    runDB $ delete articleId
    redirect EditBlogR

getDeleteArticleR :: ArticleId -> Handler Html
getDeleteArticleR articleId = do
    runDB $ delete articleId
    redirect EditBlogR

deleteDeleteArticleR :: ArticleId -> Handler Html
deleteDeleteArticleR articleId = do
    runDB $ delete articleId
    redirect EditBlogR

getEditArticleR :: ArticleId -> Handler Html
getEditArticleR articleId = do
    article <- runDB $ get404 articleId
    (articleWidget, enctype) <- generateFormPost $ articleForm (Just article)
    defaultLayout $ do
        let route = BlogR
        $(widgetFile "menu")
        $(widgetFile "edit-article")

postEditArticleR :: ArticleId -> Handler Html
postEditArticleR articleId = do
    ((res,articleWidget),enctype) <- runFormPost $ articleForm Nothing
    case res of
        FormSuccess article -> do
            runDB $ updateArticle articleId article
            redirect (ArticleR articleId)
        _ -> defaultLayout $ do
            -- redirect BlogR
            setTitle "Please correct your entry form"
            $(widgetFile "articleAddError")

