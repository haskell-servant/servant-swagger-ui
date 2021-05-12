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
-- Copyright   :  (C) 2016-2018 Oleg Grenrus
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- Provides 'SwaggerUI' and corresponding 'redocSchemaUIServer' to embed
-- <https://github.com/Rebilly/ReDoc ReDoc swagger ui> into the application.
--
-- All of the UI files are embedded into the binary.
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
-- server = 'redocSchemaUIServer' swaggerDoc
--     :\<|> (pure "Hello World" :\<|> catEndpoint)
--   where
--     catEndpoint name = pure $ Cat name False
-- @

module Servant.Swagger.UI.ReDoc (
    -- * Swagger UI API
    SwaggerSchemaUI,
    SwaggerSchemaUI',
    redocSchemaUIServer,
    redocSchemaUIServerT,
    redocSchemaUIServer',
    redocSchemaUIServerT',

    -- ** ReDoc theme
    redocIndexTemplate,
    redocFiles
    ) where

import Servant.Swagger.UI.Core

import Data.ByteString (ByteString)
import Data.Text       (Text)
import FileEmbedLzma
import Servant

-- | Serve alternative Swagger UI.
--
-- See <https://github.com/Rebilly/ReDoc/tree/v1.x>
redocSchemaUIServer
    :: (Server api ~ Handler a)
    => a -> Server (SwaggerSchemaUI' dir api)
redocSchemaUIServer =
    swaggerSchemaUIServerImpl redocIndexTemplate redocFiles

-- | Serve Redoc Swagger UI on @/dir@ using @api@ as a Swagger spec source.
--
-- Generalized to 'ServerT'
--
-- @
-- redocSchemaUIServerT :: Swagger -> ServerT (SwaggerSchemaUI schema dir) m
-- @
redocSchemaUIServerT
    :: (Monad m, ServerT api m ~ m a)
    => a -> ServerT (SwaggerSchemaUI' dir api) m
redocSchemaUIServerT =
    swaggerSchemaUIServerImpl redocIndexTemplate redocFiles

-- | Use a custom server to serve the Swagger spec source.
redocSchemaUIServer'
    :: Server api -> Server (SwaggerSchemaUI' dir api)
redocSchemaUIServer' =
    swaggerSchemaUIServerImpl' redocIndexTemplate redocFiles

-- | Use a custom server to serve the Redoc Swagger spec source.
--
-- This allows even more control over how the spec source is served.
-- It allows, for instance, serving the spec source with authentication,
-- customizing the response based on the client or serving a swagger.yaml
-- instead.
--
-- Generalized to 'ServerT'
--
redocSchemaUIServerT'
    :: Monad m => ServerT api m -> ServerT (SwaggerSchemaUI' dir api) m
redocSchemaUIServerT' =
    swaggerSchemaUIServerImpl' redocIndexTemplate redocFiles

redocIndexTemplate :: Text
redocIndexTemplate = $(embedText "redoc.index.html.tmpl")

redocFiles :: [(FilePath, ByteString)]
redocFiles = $(embedRecursiveDir "redoc-dist-1.22.3")
