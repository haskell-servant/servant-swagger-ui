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
-- Provides 'SwaggerUI' and corresponding 'swaggerSchemaUIServer' to embed
-- <http://swagger.io/swagger-ui/ swagger ui> into the application.
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

    -- ** Official swagger ui
    swaggerUiIndexTemplate,
    swaggerUiFiles,
    ) where

import Servant.Swagger.UI.Core

import Data.ByteString (ByteString)
import Data.Swagger    (Swagger)
import Data.Text       (Text)
import FileEmbedLzma
import Servant

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

swaggerUiIndexTemplate :: Text
swaggerUiIndexTemplate = $(embedText "index.html.tmpl")

swaggerUiFiles :: [(FilePath, ByteString)]
swaggerUiFiles = $(embedRecursiveDir "swagger-ui-dist-3.13.2")
