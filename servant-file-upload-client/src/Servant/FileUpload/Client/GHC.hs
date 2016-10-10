{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Servant.FileUpload.Client.GHC where

import Data.Proxy (Proxy (..))
import Data.Void (Void, absurd)
import Servant.API ((:>))
import Servant.Client (HasClient (..))
import Servant.FileUpload.API

instance (HasClient sublayout)
      => HasClient (MultiPartBody a :> sublayout) where

    type Client (MultiPartBody a :> sublayout) = Void -> Client sublayout

    clientWithRoute Proxy req void = absurd void
