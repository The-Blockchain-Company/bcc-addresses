-- |
-- Copyright: © 2018-2021 The-Blockchain-Company
-- License: Apache-2.0

module Bcc.Address.JSAPI
    ( startApi
    ) where

import Prelude

import Bcc.Address.Compat
    ( ghcjsBuildSupport )
import Control.Monad.IO.Class
    ( MonadIO (..) )
import Language.Javascript.JSaddle
    ( JSM, Object, obj )

import qualified Bcc.Address.JSAPI.InspectAddress as InspectAddress
import qualified Bcc.Address.JSAPI.Version as Version

startApi :: JSM Object
startApi = do
  liftIO ghcjsBuildSupport
  api <- obj
  Version.export api
  InspectAddress.export api
  pure api
