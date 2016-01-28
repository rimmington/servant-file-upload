{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Servant.FileUpload.Client where

import Control.DeepSeq (NFData)
import Data.Aeson (ToJSON, encode)
import Data.ByteString.Lazy (toStrict)
import qualified Data.JSString as JSS
import Data.Text (unpack)
import Data.Text.Encoding (decodeUtf8)
import Data.Proxy (Proxy (..))
import GHC.Generics (Generic)
import GHCJS.Marshal (toJSRef)
import GHCJS.Types (JSRef)
import Servant.API ((:>))
import Servant.Client (HasClient (..))
import Servant.Common.Req (FormDataProp (..), setRQFormData)
import Servant.FileUpload.API
import Servant.FileUpload.Internal (primaryBodyKey)

data File = File String JSRef String
          deriving (Generic, NFData)

instance (MultiPartData a, ToJSON (PrimaryBody a), HasClient sublayout)
      => HasClient (MultiPartBody a :> sublayout) where

    type Client (MultiPartBody a :> sublayout) = a File -> Client sublayout

    clientWithRoute Proxy req baseurl body =
        clientWithRoute (Proxy :: Proxy sublayout)
                        (setRQFormData fd req)
                        baseurl
      where
        (pb, fs) = toMultiPartData body
        pbe = unpack . decodeUtf8 $ encodeStrict pb
        fd = [FormDataString primaryBodyKey pbe] ++ (mkFDB <$> fs)
        mkFDB (File n r fn) = FormDataBlob n r fn
        encodeStrict = toStrict . encode

asFile :: String -> JSRef -> IO File
asFile n ref = File n ref . JSS.unpack <$> js_fileName ref

foreign import javascript unsafe "$1.name"
    js_fileName :: JSRef -> IO JSS.JSString