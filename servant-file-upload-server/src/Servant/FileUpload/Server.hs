{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Servant.FileUpload.Server (
      UploadedFile, inputName, fileInfo, fileName, fileContentType, filePath
    , TempFileBacked, withTempFiles, handleTempFiles
    , module Servant.FileUpload.API
    ) where

import Control.Monad.Except (ExceptT (..), runExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT, withInternalState)
import Data.Aeson (FromJSON, eitherDecodeStrict)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Data.Proxy (Proxy (..))
import Lens.Micro (Lens', lens)
import Lens.Micro.TH (makeLenses)
import Network.HTTP.Types.Header (hContentType)
import Network.Wai (Request, requestHeaders)
import Network.Wai.Parse (FileInfo, tempFileBackEnd, parseRequestBodyEx, defaultParseRequestBodyOptions)
import qualified Network.Wai.Parse as P
import Path (Path, Abs, File, parseAbsFile)
import Servant (HasServer (..), (:>))
import Servant.FileUpload.API (MultiPartData (..), MultiPartBody)
import Servant.FileUpload.Internal (primaryBodyKey)
import Servant.Server (Handler, err400, err415, err500, errBody)
import Servant.Server.Internal (addBodyCheck, withRequest, delayedFailFatal)

data UploadedFile = UploadedFile { _inputName :: ByteString, _fileInfo :: FileInfo (Path Abs File) }
                  deriving (Show, Eq)

data TempFileBacked a = TempFileBacked (forall b. (a UploadedFile -> Handler b) -> Handler b)

withTempFiles :: TempFileBacked a -> (a UploadedFile -> Handler b) -> Handler b
withTempFiles (TempFileBacked c) = c

handleTempFiles :: (a UploadedFile -> Handler b) -> TempFileBacked a -> Handler b
handleTempFiles f t = withTempFiles t f

$(makeLenses ''UploadedFile)

fileName :: Lens' UploadedFile ByteString
fileName = lens (P.fileName . _fileInfo)
                (\s b -> s { _fileInfo = (_fileInfo s) { P.fileName = b } })

fileContentType :: Lens' UploadedFile ByteString
fileContentType = lens (P.fileContentType . _fileInfo)
                       (\s b -> s { _fileInfo = (_fileInfo s) { P.fileContentType = b } })

filePath :: Lens' UploadedFile (Path Abs File)
filePath = lens (P.fileContent . _fileInfo)
                (\s b -> s { _fileInfo = (_fileInfo s) { P.fileContent = b } })

instance (MultiPartData a, FromJSON (PrimaryBody a), HasServer api context)
      => HasServer (MultiPartBody a :> api) context where

    type ServerT (MultiPartBody a :> api) m =
        TempFileBacked a -> ServerT api m

    route Proxy context subserver = route (Proxy :: Proxy api) context $ addBodyCheck subserver bodyCheck
      where
        bodyCheck = withRequest $ \req -> do
            let contentTypeH = P.parseContentType <$> lookup hContentType (requestHeaders req)
            case contentTypeH of
                Just ("multipart/form-data", _) -> pure $ go req
                _                               -> delayedFailFatal err415
        withBackend f = runResourceT . withInternalState $ f . tempFileBackEnd
        go :: Request -> TempFileBacked a
        go req = TempFileBacked $ \f -> ExceptT . withBackend $ \b -> runExceptT $ do
            (params, fs) <- liftIO $ parseRequestBodyEx defaultParseRequestBodyOptions b req
            bdy <- withEither mk400 $ parseBody params
            fs' <- withEither (const $ err500 { errBody = "Uploaded file error" }) $ traverse parseFileName fs
            a <- withEither mk400 $ parseMultiPartData bdy fs'
            f a
        mk400 e = err400 { errBody = BSL8.pack e }
        withEither f = ExceptT . pure . first f
        parseBody params = do
            json <- maybe (Left "Missing primary body") Right $ lookup primaryBodyKeyBS params
            eitherDecodeStrict json :: Either String (PrimaryBody a)
        parseFileName (n, P.FileInfo{..}) = do
            path <- parseAbsFile fileContent
            pure . UploadedFile n $ P.FileInfo{fileContent = path, ..}

primaryBodyKeyBS :: ByteString
primaryBodyKeyBS = BS8.pack primaryBodyKey
