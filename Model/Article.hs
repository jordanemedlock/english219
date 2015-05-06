module Model.Article where

import Import
import Yesod.Form.Bootstrap3
import Yesod.Markdown

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
<a href=/static/img/#{url} .thumbnail>
  <img src=/static/img/#{url} .img-responsive.img-rounded>
|]

updateArticle :: ArticleId -> Article -> YesodDB App ()
updateArticle articleId article = updateWhere 
                                    [ ArticleId ==. articleId ] 
                                    [ ArticleTitle =. (articleTitle article)
                                    , ArticlePreviewImage =. (articlePreviewImage article)
                                    , ArticlePreviewText =. (articlePreviewText article)
                                    , ArticleContent =. (articleContent article)
                                    , ArticleTimeEdited =. (articleTimeEdited article)
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
      <div .row>
          <div .col-md-6>
              #{markdownToHtml $ articlePreviewText article}
              <a href=@{ArticleR articleId}>
                  Read More
              $if edit 
                <br>
                ^{editArticle articleId}
                <br>
                ^{deleteArticle articleId}
          <div .col-md-6>
              ^{toImage $ articlePreviewImage article}
|]