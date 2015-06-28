module Handler.Blog (
    getBlogR,
    getEditBlogR,
    postEditBlogR,
    getJSONBlogR,
    postJSONBlogR
) where

import Import
import Yesod.Form.Bootstrap3
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
    (photoWidget, enctype2) <- generateFormPost $ photoUploadForm
    defaultLayout $ do
        let color = "red"
        setTitle "Jordan E Medlock"
        $(widgetFile "articles")

photoUploadForm :: Form (FileInfo, Text)
photoUploadForm = renderBootstrap3 bootstrapSettings $ (,)
    <$> areq fileField (bfs ("Photo File" :: Text)) Nothing
    <*> areq textField (bfs ("File Name" :: Text)) Nothing
    <*  bootstrapSubmit ("Upload" :: BootstrapSubmit Text)
    
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

                
getJSONBlogR :: Handler Html
getJSONBlogR = error "Not yest implemented"

postJSONBlogR :: Handler Html
postJSONBlogR = error "Not yest implemented"