module Handler.UploadPhoto where

import Import
import Yesod.Form.Bootstrap3

postUploadPhotoR :: Handler Html
postUploadPhotoR = do
    ((res,_),enctype) <- runFormPost $ photoUploadForm
    case res of
        FormSuccess (fileInfo, fileName) -> do
            liftIO $ fileMove fileInfo (unpack $ "static/img/"<>fileName)
            redirect BlogR
        _ -> redirect BlogR


bootstrapSettings :: BootstrapFormLayout
bootstrapSettings = BootstrapHorizontalForm (ColMd 0) (ColMd 4) (ColMd 0) (ColMd 8)


photoUploadForm :: Form (FileInfo, Text)
photoUploadForm = renderBootstrap3 bootstrapSettings $ (,)
    <$> areq fileField (bfs ("Photo File" :: Text)) Nothing
    <*> areq textField (bfs ("File Name" :: Text)) Nothing
