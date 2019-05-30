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
    redocSchemaUIServer',

    -- ** ReDoc theme
    redocIndexTemplate,
    redocFiles
    ) where

import Servant.Swagger.UI.Core

import Data.ByteString (ByteString)
import Data.Swagger    (Swagger)
import Data.Text       (Text)
import FileEmbedLzma
import Servant

-- | Serve alternative Swagger UI.
--
-- See <https://github.com/Rebilly/ReDoc/tree/v1.x>
redocSchemaUIServer
    :: (Server api ~ Handler Swagger)
    => Swagger -> Server (SwaggerSchemaUI' dir api)
redocSchemaUIServer =
    swaggerSchemaUIServerImpl redocIndexTemplate redocFiles

-- | Use a custom server to serve the Swagger spec source.
redocSchemaUIServer'
    :: Server api -> Server (SwaggerSchemaUI' dir api)
redocSchemaUIServer' =
    swaggerSchemaUIServerImpl' redocIndexTemplate redocFiles


redocIndexTemplate :: Text
redocIndexTemplate = $(embedText "redoc.index.html.tmpl")

redocFiles :: [(FilePath, ByteString)]
redocFiles = $(embedRecursiveDir "redoc-dist-1.22.2")
