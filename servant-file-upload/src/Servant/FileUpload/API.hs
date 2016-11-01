{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Servant.FileUpload.API where

import Data.Proxy (Proxy (..))
import Servant.API ((:>))
import Servant.Utils.Links (HasLink (..))

data MultiPartBody (b :: * -> *)

class MultiPartData (a :: * -> *) where
    type PrimaryBody a :: *
    parseMultiPartData :: PrimaryBody a -> [b] -> Either String (a b)
    toMultiPartData :: a b -> (PrimaryBody a, [b])

instance (HasLink sub) => HasLink (MultiPartBody a :> sub) where
    type MkLink (MultiPartBody a :> sub) = MkLink sub
    toLink _ = toLink (Proxy :: Proxy sub)
