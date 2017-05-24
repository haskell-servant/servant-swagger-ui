{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Servant.Swagger.UI
-- Copyright   :  (C) 2016 Oleg Grenrus
-- License     :  BSD3
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- Provides 'SwaggerUI' and corresponding 'swaggerUIServer' to embed
-- <http://swagger.io/swagger-ui/ swagger ui> into the application.
--
-- All of UI files are embedded into the binary.
--
-- /An example:/
--
-- @
-- -- | Actual API.
-- type BasicAPI = Get '[PlainText, JSON] Text
--     :\<|> "cat" :> Capture ":name" CatName :> Get '[JSON] Cat
--
-- -- | API type with bells and whistles, i.e. schema file and swagger-ui.
-- type API = 'SwaggerSchemaUI' "swagger-ui" "swagger.json"
--     :\<|> BasicAPI
--
-- -- | Servant server for an API
-- server :: Server API
-- server = 'swaggerSchemaUIServer' swaggerDoc
--     :\<|> (pure "Hello World" :\<|> catEndpoint)
--   where
--     catEndpoint name = pure $ Cat name False
-- @

module Servant.Swagger.UI (
    -- * Swagger UI API
    SwaggerSchemaUI,
    SwaggerSchemaUI',
    swaggerSchemaUIServer,
    jensolegSwaggerSchemaUIServer,
    -- * Internals
    --
    -- /Note:/ in next major version, these will be moved to separate module.
    SwaggerUiHtml(..),
    swaggerSchemaUIServerImpl,
    -- ** Official swagger ui
    swaggerUiIndexTemplate,
    swaggerUiFiles,
    -- ** jensoleg theme
    --
    -- Current version: @79f3bba07b070cfab1d8c245c4f9229052e20a1a@
    jensolegIndexTemplate,
    jensolegFiles,
    ) where

import Data.ByteString                (ByteString)
import Data.FileEmbed                 (embedStringFile)
import Data.Swagger                   (Swagger)
import GHC.TypeLits                   (KnownSymbol, Symbol, symbolVal)
import Network.Wai.Application.Static (embeddedSettings, staticApp)
import Servant
import Servant.HTML.Blaze             (HTML)
import Servant.Swagger.UI.Internal
import Text.Blaze                     (ToMarkup (..))

import qualified Data.Text as T

#if MIN_VERSION_servant(0,7,0)
-- do nothing
#else
#if MIN_VERSION_servant(0,5,0)
import Control.Monad.Trans.Except (ExceptT)
#define Handler ExceptT ServantErr IO
#else
import Control.Monad.Trans.Either (EitherT)
#define Handler EitherT ServantErr IO
#endif
#endif

-- | Swagger schema + ui api.
--
-- @SwaggerSchemaUI "swagger-ui" "swagger.json"@ will result into following hierarchy:
--
-- @
-- \/swagger.json
-- \/swagger-ui
-- \/swagger-ui\/index.html
-- \/swagger-ui\/...
-- @
--
type SwaggerSchemaUI (dir :: Symbol) (schema :: Symbol) =
    SwaggerSchemaUI' dir (schema :> Get '[JSON] Swagger)

-- | Use 'SwaggerSchemaUI'' when you need even more control over
-- where @swagger.json@ is served (e.g. subdirectory).
type SwaggerSchemaUI' (dir :: Symbol) (api :: *) =
    api
    :<|> dir :>
        ( Get '[HTML] (SwaggerUiHtml dir api)
        :<|> "index.html" :> Get '[HTML] (SwaggerUiHtml dir api)
        :<|> Raw
        )

-- | Index file for swagger ui.
--
-- It's configured by the location of swagger schema and directory it lives under.
--
-- Implementation detail: the @index.html@ is prepopulated with parameters
-- to find schema file automatically.
data SwaggerUiHtml (dir :: Symbol) (api :: *) = SwaggerUiHtml T.Text

#if MIN_VERSION_servant(0,10,0)
#define LINK Link
#define LINKPATH uriPath . linkURI
#else
#define LINK URI
#define LINKPATH uriPath
#endif

instance (KnownSymbol dir, HasLink api, LINK ~ MkLink api, IsElem api api)
    => ToMarkup (SwaggerUiHtml dir api)
  where
    toMarkup (SwaggerUiHtml template) = preEscapedToMarkup
        $ T.replace "SERVANT_SWAGGER_UI_SCHEMA" schema
        $ T.replace "SERVANT_SWAGGER_UI_DIR" dir
        $ template
      where
        schema = T.pack $ LINKPATH $ safeLink proxyApi proxyApi
        dir    = T.pack $ symbolVal (Proxy :: Proxy dir)
        proxyApi = Proxy :: Proxy api

-- | Serve Swagger UI on @/dir@ using @api@ as a Swagger spec source.
--
-- @
-- swaggerSchemaUIServer :: Swagger -> Server (SwaggerSchemaUI schema dir)
-- @
swaggerSchemaUIServer
    :: (Server api ~ Handler Swagger)
    => Swagger -> Server (SwaggerSchemaUI' dir api)
swaggerSchemaUIServer =
    swaggerSchemaUIServerImpl swaggerUiIndexTemplate swaggerUiFiles

-- | Serve alternative Swagger UI.
--
-- See <https://github.com/jensoleg/swagger-ui>
jensolegSwaggerSchemaUIServer
    :: (Server api ~ Handler Swagger)
    => Swagger -> Server (SwaggerSchemaUI' dir api)
jensolegSwaggerSchemaUIServer =
    swaggerSchemaUIServerImpl jensolegIndexTemplate jensolegFiles

swaggerSchemaUIServerImpl
    :: (Server api ~ Handler Swagger)
    => T.Text -> [(FilePath, ByteString)]
    -> Swagger -> Server (SwaggerSchemaUI' dir api)
swaggerSchemaUIServerImpl indexTemplate files swagger = return swagger
    :<|> return (SwaggerUiHtml indexTemplate)
    :<|> return (SwaggerUiHtml indexTemplate)
    :<|> rest
  where
    rest =
#if MIN_VERSION_servant_server(0,11,0)
        Tagged $
#endif
        staticApp $ embeddedSettings files

swaggerUiIndexTemplate :: T.Text
swaggerUiIndexTemplate = $(embedStringFile "index.html.tmpl")

swaggerUiFiles :: [(FilePath, ByteString)]
swaggerUiFiles = $(mkRecursiveEmbedded "swagger-dist-2.2.8")

jensolegIndexTemplate :: T.Text
jensolegIndexTemplate = $(embedStringFile "jensoleg.index.html.tmpl")

jensolegFiles :: [(FilePath, ByteString)]
jensolegFiles = $(mkRecursiveEmbedded "jensoleg-dist")
