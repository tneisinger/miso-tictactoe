module Common.Routes where

import Data.Proxy (Proxy(..))
import Miso (View)
import qualified Servant.API as Servant
import qualified Servant.Utils.Links as Servant

import Common.Model

-- Holds a servant route tree of `View action`
type ViewRoutes = Home

-- Home route
type Home = View Action

-- Servant.URI that points to the home route
homeLink :: Servant.URI
homeLink = Servant.linkURI
  $ Servant.safeLink (Proxy :: Proxy ViewRoutes) (Proxy :: Proxy Home)
