{-# LANGUAGE CPP #-}

module Servant.FileUpload.Client (
    module X
    ) where

#ifdef OnGhcjs
import Servant.FileUpload.Client.GHCJS as X
#else
import Servant.FileUpload.Client.GHC   as X
#endif
