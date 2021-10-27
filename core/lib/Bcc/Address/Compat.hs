{-# LANGUAGE CPP #-}

-- |
-- Copyright: © 2018-2021 The-Blockchain-Company
-- License: Apache-2.0
--
-- A compatibility function for the GHCJS build.

module Bcc.Address.Compat
    ( ghcjsBuildSupport
    ) where

import Prelude

#ifdef ghcjs_HOST_OS
import Bcc.Address.Jsbits
    ( addJsbitsDependency )
#endif

-- | This function must be used somewhere, so that external Javascript files are
-- correctly linked in the GHCJS build.
--
-- For non-GHCJS, it has no effect.
ghcjsBuildSupport :: IO ()
#ifdef ghcjs_HOST_OS
ghcjsBuildSupport = addJsbitsDependency
#else
ghcjsBuildSupport = pure ()
#endif
