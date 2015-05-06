module Handler.Blog (
    getBlogR,
    getEditBlogR,
    postEditBlogR,
    YesodNic,
    nicHtmlField
) where

import Import
import Yesod.Form.Nic (YesodNic, nicHtmlField)
import Model.Article

getBlogR :: Handler Html
getBlogR = partialGetBlog False BlogR

getEditBlogR :: Handler Html 
getEditBlogR = partialGetBlog True EditBlogR

partialGetBlog :: Bool -> Route App -> Handler Html
partialGetBlog edit route = do
    -- Get the list of articles inside the database.
    articles <- runDB $ selectList [] [Desc ArticleTimePosted]
    -- We'll need the two "objects": articleWidget and enctype
    -- to construct the form (see templates/articles.hamlet).
    (articleWidget, enctype) <- generateFormPost $ articleForm Nothing
    defaultLayout $ do
        setTitle "Jordan E Medlock - Blog"
        $(widgetFile "menu")
        $(widgetFile "articles")

postEditBlogR :: Handler Html
postEditBlogR = do
    ((res,articleWidget),enctype) <- runFormPost $ articleForm Nothing
    case res of
         FormSuccess article -> do
            articleId <- runDB $ insert article
            setMessage $ toHtml $ (articleTitle article) <> " created"
            redirect $ ArticleR articleId
         _ -> defaultLayout $ do
                setTitle "Please correct your entry form"
                $(widgetFile "articleAddError")
