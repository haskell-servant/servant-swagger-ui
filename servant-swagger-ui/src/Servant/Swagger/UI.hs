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
-- Copyright   :  (C) 2016-2018 Oleg Grenrus
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- Provides 'SwaggerUI' and corresponding 'swaggerSchemaUIServer' to embed
-- <http://swagger.io/swagger-ui/ swagger ui> into the application.
--
-- All of the UI files are embedded into the binary.
--
-- Note that you need to depend on particular @swagger-ui@ compatible provider:
--
-- - <https://hackage.haskell.org/package/swagger2>
-- - <https://hackage.haskell.org/package/openapi3>
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
    swaggerSchemaUIServerT,
    swaggerSchemaUIServer',
    swaggerSchemaUIServerT',

    -- ** Official swagger ui
    swaggerUiIndexTemplate,
    swaggerUiFiles,
    ) where

import Servant.Swagger.UI.Core

import Data.Aeson      (ToJSON, Value)
import Data.ByteString (ByteString)
import qualified Data.Bifunctor as Bifunctor
import qualified Data.ByteString as BS
import Data.FileEmbed (embedDir, embedFile)
import qualified Data.List as List
import Data.Text       (Text)
import Data.Text.Encoding (decodeUtf8, decodeUtf8')
import Language.Haskell.TH (runIO)
import Servant

-- | Serve Swagger UI on @/dir@ using @api@ as a Swagger spec source.
--
-- @
-- swaggerSchemaUIServer :: Swagger -> Server (SwaggerSchemaUI schema dir)
-- swaggerSchemaUIServer :: OpenApi -> Server (SwaggerSchemaUI schema dir)
-- @
swaggerSchemaUIServer
    :: (Server api ~ Handler Value, ToJSON a)
    => a -> Server (SwaggerSchemaUI' dir api)
swaggerSchemaUIServer =
    swaggerSchemaUIServerImpl swaggerUiIndexTemplate swaggerUiFiles

-- | Serve Swagger UI on @/dir@ using @api@ as a Swagger spec source.
--
-- Generalized to 'ServerT'
--
-- @
-- swaggerSchemaUIServerT :: Swagger -> ServerT (SwaggerSchemaUI schema dir) m
-- swaggerSchemaUIServerT :: OpenApi -> ServerT (SwaggerSchemaUI schema dir) m
-- @
swaggerSchemaUIServerT
    :: (Monad m, ServerT api m ~ m Value, ToJSON a)
    => a -> ServerT (SwaggerSchemaUI' dir api) m
swaggerSchemaUIServerT =
    swaggerSchemaUIServerImpl swaggerUiIndexTemplate swaggerUiFiles

-- | Use a custom server to serve the Swagger spec source.
--
-- This allows even more control over how the spec source is served.
-- It allows, for instance, serving the spec source with authentication,
-- customizing the response based on the client or serving a swagger.yaml
-- instead.
swaggerSchemaUIServer'
    :: Server api -> Server (SwaggerSchemaUI' dir api)
swaggerSchemaUIServer' =
    swaggerSchemaUIServerImpl' swaggerUiIndexTemplate swaggerUiFiles

-- | Use a custom server to serve the Swagger spec source.
--
-- This allows even more control over how the spec source is served.
-- It allows, for instance, serving the spec source with authentication,
-- customizing the response based on the client or serving a swagger.yaml
-- instead.
--
-- Generalized to 'ServerT'
--
swaggerSchemaUIServerT'
    :: Monad m => ServerT api m -> ServerT (SwaggerSchemaUI' dir api) m
swaggerSchemaUIServerT' =
    swaggerSchemaUIServerImpl' swaggerUiIndexTemplate swaggerUiFiles

swaggerUiIndexTemplate :: Text
swaggerUiIndexTemplate = $(do
    let path = "index.html.tmpl"
    contents <- runIO (BS.readFile path)
    either (fail . show) (const [| decodeUtf8 $(embedFile path) |]) (decodeUtf8' contents))

swaggerUiFiles :: [(FilePath, ByteString)]
swaggerUiFiles =
    List.sortOn fst . fmap (Bifunctor.first ('/' :)) $ $(embedDir "swagger-ui-dist-5.31.0")
