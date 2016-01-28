{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Servant.FileUpload.Server (
      UploadedFile, inputName, fileInfo, fileName, fileContentType, filePath
    , module Servant.FileUpload.API
    ) where

import Control.Monad.Trans.Resource (runResourceT, withInternalState)
import Data.Aeson (FromJSON, eitherDecodeStrict)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Data.Proxy (Proxy (..))
import Data.Text.Encoding (encodeUtf8)
import Lens.Micro (Lens', lens)
import Lens.Micro.TH (makeLenses)
import Network.Wai.Parse (FileInfo, tempFileBackEnd, parseRequestBody)
import qualified Network.Wai.Parse as P
import Servant (HasServer (..), ServantErr (..), (:>), err400)
import Servant.FileUpload.API (MultiPartData (..), MultiPartBody)
import Servant.FileUpload.Internal (primaryBodyKey)
import Servant.Server.Internal (succeedWith)
import Servant.Server.Internal.ServantErr (responseServantErr)

data UploadedFile = UploadedFile { _inputName :: ByteString, _fileInfo :: FileInfo FilePath }

$(makeLenses ''UploadedFile)

fileName :: Lens' UploadedFile ByteString
fileName = lens (P.fileName . _fileInfo)
                (\s b -> s { _fileInfo = (_fileInfo s) { P.fileName = b } })

fileContentType :: Lens' UploadedFile ByteString
fileContentType = lens (P.fileContentType . _fileInfo)
                       (\s b -> s { _fileInfo = (_fileInfo s) { P.fileContentType = b } })

filePath :: Lens' UploadedFile FilePath
filePath = lens (P.fileContent . _fileInfo)
                (\s b -> s { _fileInfo = (_fileInfo s) { P.fileContent = b } })

instance (MultiPartData a, FromJSON (PrimaryBody a), HasServer sublayout)
      => HasServer (MultiPartBody a :> sublayout) where

    type ServerT (MultiPartBody a :> sublayout) m =
        a UploadedFile -> ServerT sublayout m

    route Proxy subserver req respond = withBackend $ \b -> do
        (params, fs) <- parseRequestBody b req
        case parse params fs of
            Left e   -> respond . succeedWith . responseServantErr $ err400 { errBody = BSL8.pack e }
            Right sa -> route (Proxy :: Proxy sublayout) (subserver sa) req respond
      where
        withBackend f = runResourceT . withInternalState $ f . tempFileBackEnd
        parse params fs = do
            json <- maybe (Left "Missing primary body") Right $ lookup primaryBodyKeyBS params
            a <- eitherDecodeStrict json :: Either String (PrimaryBody a)
            parseMultiPartData a (uncurry UploadedFile <$> fs)

primaryBodyKeyBS :: ByteString
primaryBodyKeyBS = BS8.pack primaryBodyKey
