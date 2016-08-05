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
-- type API = 'SwaggerSchemaUI' "swagger.json" "swagger-ui"
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
    -- * Internals
    SwaggerUiHtml(..),
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

#if MIN_VERSION_servant(0,5,0)
import Control.Monad.Trans.Except (ExceptT)
#else
import Control.Monad.Trans.Either (EitherT)
#define ExceptT EitherT
#endif

-- | Swagger schema + ui api.
--
-- @SwaggerSchemaUI "swagger.json" "swagger-ui"@ will result into following hierarchy:
--
-- @
-- \/swagger.json
-- \/swagger-ui
-- \/swagger-ui\/index.html
-- \/swagger-ui\/...
-- @
--
type SwaggerSchemaUI (schema :: Symbol) (dir :: Symbol) =
    SwaggerSchemaUI' (schema :> Get '[JSON] Swagger) dir

-- | Use 'SwaggerSchemaUI'' when you need even more control over
-- where @swagger.json@ is served (e.g. subdirectory).
type SwaggerSchemaUI' (api :: *) (dir :: Symbol) =
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
data SwaggerUiHtml (dir :: Symbol) (api :: *) = SwaggerUiHtml

instance (KnownSymbol dir, HasLink api, URI ~ MkLink api, IsElem api api)
    => ToMarkup (SwaggerUiHtml dir api)
  where
    toMarkup _ = preEscapedToMarkup
        $ T.replace "SERVANT_SWAGGER_UI_SCHEMA" schema
        $ T.replace "SERVANT_SWAGGER_UI_DIR" dir
        $ swaggerUiIndexTemplate
      where
        schema = T.pack $ uriPath $ safeLink proxyApi proxyApi
        dir    = T.pack $ symbolVal (Proxy :: Proxy dir)
        proxyApi = Proxy :: Proxy api

-- | Serve Swagger UI on @/dir@ using @api@ as a Swagger spec source.
--
-- @
-- swaggerSchemaUIServer :: Swagger -> Server (SwaggerSchemaUI schema dir)
-- @
swaggerSchemaUIServer
    :: (Server api ~ ExceptT ServantErr IO Swagger)
    => Swagger -> Server (SwaggerSchemaUI' api dir)
swaggerSchemaUIServer swagger = return swagger
    :<|> return SwaggerUiHtml
    :<|> return SwaggerUiHtml
    :<|> rest
  where
    rest = staticApp $ embeddedSettings swaggerUiFiles

swaggerUiIndexTemplate :: T.Text
swaggerUiIndexTemplate = $(embedStringFile "index.html.tmpl")

swaggerUiFiles :: [(FilePath, ByteString)]
swaggerUiFiles = $(mkRecursiveEmbedded "swagger-dist-2.1.5")
