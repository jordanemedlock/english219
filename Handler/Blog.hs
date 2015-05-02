module Handler.Blog (
    getBlogR,
    postBlogR,
    YesodNic,
    nicHtmlField
) where

import Import
import Yesod.Form.Nic (YesodNic, nicHtmlField)
import Text.Blaze 


instance YesodNic App

data GridOptions = ColXs Int | ColSm Int | ColMd Int | ColLg Int

instance Show GridOptions where
    show (ColXs 0) = ""
    show (ColXs columns) = "col-xs-" ++ show columns

    show (ColSm 0) = ""
    show (ColSm columns) = "col-sm-" ++ show columns

    show (ColMd 0) = ""
    show (ColMd columns) = "col-md-" ++ show columns

    show (ColLg 0) = ""
    show (ColLg columns) = "col-lg-" ++ show columns


instance ToMarkup GridOptions where
    toMarkup = toMarkup . show

data BootstrapFormConfig = BootstrapFormConfig { form :: BootstrapForm, submit :: String }

data BootstrapForm = BootstrapBasicForm | BootstrapInlineForm
    | BootstrapHorizontalForm { containerOffset :: GridOptions, container :: GridOptions, label :: GridOptions }

hConfig = BootstrapFormConfig { form = BootstrapBasicForm 
                              , submit = "Submit" }


toOffset :: GridOptions -> String
toOffset (ColXs 0) = ""
toOffset (ColSm 0) = ""
toOffset (ColMd 0) = ""
toOffset (ColLg 0) = ""
toOffset (ColXs columns) = "col-xs-offset-" ++ show columns
toOffset (ColSm columns) = "col-sm-offset-" ++ show columns
toOffset (ColMd columns) = "col-md-offset-" ++ show columns
toOffset (ColLg columns) = "col-lg-offset-" ++ show columns

bootstrapFieldSettings :: BootstrapFormConfig -> 
                          SomeMessage site -> 
                          Maybe (SomeMessage site) -> 
                          Maybe Text -> 
                          Maybe Text -> 
                          Maybe Text -> 
                          FieldSettings site
bootstrapFieldSettings formConfig msg tooltip placeholder id name =
    FieldSettings msg tooltip id name (attrsFromFormConfig formConfig placeholder)

attrsFromFormConfig :: BootstrapFormConfig -> Maybe Text -> [(Text, Text)]
attrsFromFormConfig _ Nothing = [("class", "form-control")]
attrsFromFormConfig _ (Just placeholder) = [("class", "form-control"), ("placeholder", placeholder)]

helpWidget view = [whamlet|
    $maybe tt <- fvTooltip view
      <span .help-block>#{tt}
    $maybe err <- fvErrors view
      <span .help-block>#{err}
|]

submitWidget (BootstrapFormConfig (BootstrapHorizontalForm containerOffset containerClass labelClass) submit) = [whamlet|
<div .form-group>
    <div .#{toOffset containerOffset} .#{containerClass}>
      <button type=submit .btn .btn-default>#{submit}
|]
submitWidget (BootstrapFormConfig _ submit) = [whamlet|<button type=submit .btn .btn-default>#{submit}|]


renderBootstrap3 :: Monad m => BootstrapFormConfig -> FormRender m a
renderBootstrap3 formConfig aform fragment = do
    (res, views') <- aFormToForm aform
    let views = views' []
        has (Just _) = True
        has Nothing  = False
    let widget = $(widgetFile "bootstrap-form")
    return (res, widget)

myMarkdownEditor :: Monad m => RenderMessage (HandlerSite m) FormMessage => Field m Text
myMarkdownEditor = Field
    { fieldParse = parseHelper $ Right
    , fieldView = \theId name attrs val _isReq -> toWidget [hamlet|
$newline never
<textarea id="#{theId}" name="#{name}" *{attrs}>#{either id id val}
|]
    , fieldEnctype = UrlEncoded
    }

entryForm :: Form Article
entryForm = renderBootstrap3 hConfig $ Article
    <$> areq   textField (bootstrapFieldSettings hConfig "Title" Nothing (Just "Title") Nothing Nothing) Nothing
    <*> areq   myMarkdownEditor (bootstrapFieldSettings hConfig "Content" Nothing (Just "Content") Nothing Nothing) Nothing








getBlogR :: Handler Html
getBlogR = do
    -- Get the list of articles inside the database.
    articles <- runDB $ selectList [] [Desc ArticleTitle]
    -- We'll need the two "objects": articleWidget and enctype
    -- to construct the form (see templates/articles.hamlet).
    (articleWidget, enctype) <- generateFormPost entryForm
    defaultLayout $ do
        $(widgetFile "articles")

postBlogR :: Handler Html
postBlogR = do
    ((res,articleWidget),enctype) <- runFormPost entryForm
    case res of
         FormSuccess article -> do
            articleId <- runDB $ insert article
            setMessage $ toHtml $ (articleTitle article) <> " created"
            redirect $ ArticleR articleId
         _ -> defaultLayout $ do
                setTitle "Please correct your entry form"
                $(widgetFile "articleAddError")
