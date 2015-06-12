module Model.Article where

import Import
import Yesod.Form.Bootstrap3
import Yesod.Markdown
import Data.Aeson
import Data.Aeson.Encode.Pretty

instance FromJSON Article where
  parseJSON (Object o) = Article <$> o .: "title"
                                 <*> o .: "image"
                                 <*> (Markdown <$> o .: "preview")
                                 <*> (Markdown <$> o .: "body")
                                 <*> o .: "time_posted"
                                 <*> o .: "time_edited"
                                 <*> o .:? "posted"
  parseJSON _ = mzero

instance ToJSON Article where
  toJSON (Article t i p b tp te po) = object [ "title" .= t
                                             , "image" .= i
                                             , "preview" .= unMarkdown p
                                             , "body" .= unMarkdown b
                                             , "time_posted" .= tp
                                             , "time_edited" .= te
                                             , "posted" .= po 
                                             ] 

articleJson :: Article -> String
articleJson = map (toEnum . fromEnum) . unpack . encodePretty

jsonArticle :: String -> Maybe Article
jsonArticle = decode . pack . map (toEnum . fromEnum)


bootstrapSettings :: BootstrapFormLayout
bootstrapSettings = BootstrapHorizontalForm (ColMd 0) (ColMd 4) (ColMd 0) (ColMd 8)

articleForm :: Maybe Article -> Form Article
articleForm marticle = renderBootstrap3 bootstrapSettings $ Article
    <$> areq textField (bfs ("Title" :: Text)) (articleTitle <$> marticle)
    <*> areq textField (bfs ("Preview Image" :: Text)) (articlePreviewImage <$> marticle)
    <*> areq markdownField "Preview Text" { fsAttrs = [("class", "form-control markdown preview")] }  (articlePreviewText <$> marticle)
    <*> areq markdownField "Content" { fsAttrs = [("class", "form-control markdown")] } (articleContent <$> marticle)
    <*> case marticle of
      (Just article) -> pure $ articleTimePosted article
      Nothing -> lift $ liftIO getCurrentTime
    <*> lift (liftIO getCurrentTime)
    <*> aopt checkBoxField "Post?" (articlePosted <$> marticle)
    <*  bootstrapSubmit ("Create" :: BootstrapSubmit Text)


editArticle :: ArticleId -> WidgetT App IO ()
editArticle articleId = [whamlet| 
<a href=@{EditArticleR articleId}>
  Edit
|]

deleteArticle :: ArticleId -> WidgetT App IO ()
deleteArticle articleId = [whamlet| 
<a href=@{DeleteArticleR articleId}>
  Delete
|]

toImage :: Text -> WidgetT App IO ()
toImage url = [whamlet| 
<a href=/static/img/#{url} .thumbnail .pull-right>
  <img src=/static/img/#{url} .img-responsive.img-rounded.preview-image>
|]

updateArticle :: ArticleId -> Article -> YesodDB App ()
updateArticle articleId article = updateWhere 
                                    [ ArticleId ==. articleId ] 
                                    [ ArticleTitle =. (articleTitle article)
                                    , ArticlePreviewImage =. (articlePreviewImage article)
                                    , ArticlePreviewText =. (articlePreviewText article)
                                    , ArticleContent =. (articleContent article)
                                    , ArticleTimeEdited =. (articleTimeEdited article)
                                    , ArticlePosted =. (articlePosted article)
                                    ]

timeTag :: Article -> WidgetT App IO ()
timeTag article = [whamlet|
<small .info>
  Posted on #{formatTime defaultTimeLocale "%e %b, %Y" $ articleTimePosted article} 
  Last edited #{formatTime defaultTimeLocale "%e %b, %Y" $ articleTimeEdited article}
|]

articlePreview :: Bool -> ArticleId -> Article -> WidgetT App IO ()
articlePreview edit articleId article = [whamlet|
<div .panel .panel-default>
  <div .panel-heading>
      <h2 .panel-title>
          <a href=@{ArticleR articleId}>
              #{articleTitle article}
  <div .panel-body>
      ^{toImage $ articlePreviewImage article}
      #{markdownToHtml $ articlePreviewText article}
      <a href=@{ArticleR articleId}>
          Read More
      $if edit 
          <br>
          ^{editArticle articleId}
          <br>
          ^{deleteArticle articleId}
      
|]

betterCheckBox :: Monad m => RenderMessage (HandlerSite m) FormMessage => Field m Bool
betterCheckBox = Field
    { fieldParse = \e _ -> return $ checkBoxParser e
    , fieldView  = \theId name attrs val _ -> [whamlet|
$newline never
<input id=#{theId} *{attrs} type=checkbox name=#{name} value=yes :showVal id val:checked>
|]
    , fieldEnctype = UrlEncoded
    }

    where
        checkBoxParser [] = Right $ Just False
        checkBoxParser (x:_) = case x of
            "yes" -> Right $ Just True
            "on" -> Right $ Just True
            _     -> Right $ Just False

        showVal = either (\_ -> False)
