{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Servant.FileUpload.Client.GHCJS where

import Control.DeepSeq (NFData)
import Data.Aeson (ToJSON, encode)
import Data.ByteString.Lazy (toStrict)
import qualified Data.JSString as JSS
import Data.Text (unpack)
import Data.Text.Encoding (decodeUtf8)
import Data.Proxy (Proxy (..))
import GHC.Generics (Generic)
import GHCJS.Types (JSVal)
import JavaScript.FormData (FormDataProp (..))
import Servant.API ((:>))
import Servant.Client (HasClient (..))
import Servant.Client.GHCJS (setRQFormData)
import Servant.FileUpload.API
import Servant.FileUpload.Internal (primaryBodyKey)

data File = File String JSVal String
          deriving (Generic, NFData)

instance (MultiPartData a, ToJSON (PrimaryBody a), HasClient api)
      => HasClient (MultiPartBody a :> api) where

    type Client (MultiPartBody a :> api) = a File -> Client api

    clientWithRoute Proxy req body =
        clientWithRoute (Proxy :: Proxy api)
                        (setRQFormData fd req)
      where
        (pb, fs) = toMultiPartData body
        pbe = unpack . decodeUtf8 $ encodeStrict pb
        fd = [FormDataString primaryBodyKey pbe] ++ (mkFDB <$> fs)
        mkFDB (File n r fn) = FormDataBlob n r fn
        encodeStrict = toStrict . encode

asFile :: String -> JSVal -> IO File
asFile n val = File n val . JSS.unpack <$> js_fileName val

foreign import javascript unsafe "$1.name"
    js_fileName :: JSVal -> IO JSS.JSString
