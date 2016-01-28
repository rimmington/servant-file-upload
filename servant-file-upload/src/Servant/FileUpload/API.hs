{-# LANGUAGE TypeFamilies #-}

module Servant.FileUpload.API where

data MultiPartBody (b :: * -> *)

class MultiPartData (a :: * -> *) where
    type PrimaryBody a :: *
    parseMultiPartData :: PrimaryBody a -> [b] -> Either String (a b)
    toMultiPartData :: a b -> (PrimaryBody a, [b])
