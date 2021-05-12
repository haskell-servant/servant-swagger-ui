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
-- Provides 'SwaggerUI' and corresponding 'jensolegSwaggerSchemaUIServer' to embed
-- <https://github.com/jensoleg/swagger-org Jens-Ole Graulund themed swagger ui> into the application.
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
-- server = 'jensolegwaggerSchemaUIServer' swaggerDoc
--     :\<|> (pure "Hello World" :\<|> catEndpoint)
--   where
--     catEndpoint name = pure $ Cat name False
-- @

module Servant.Swagger.UI.JensOleG (
    -- * Swagger UI API
    SwaggerSchemaUI,
    SwaggerSchemaUI',
    jensolegSwaggerSchemaUIServer,
    jensolegSwaggerSchemaUIServer',

    -- ** ReDoc theme
    jensolegIndexTemplate,
    jensolegFiles,
    ) where

import Servant.Swagger.UI.Core

import Data.ByteString (ByteString)
import Data.Text       (Text)
import FileEmbedLzma
import Servant

-- | Serve alternative Swagger UI.
--
-- Current version: @79f3bba07b070cfab1d8c245c4f9229052e20a1a@
--
-- See <https://github.com/jensoleg/swagger-ui>
jensolegSwaggerSchemaUIServer
    :: (Server api ~ Handler a)
    => a -> Server (SwaggerSchemaUI' dir api)
jensolegSwaggerSchemaUIServer =
    swaggerSchemaUIServerImpl jensolegIndexTemplate jensolegFiles

-- | Use a custom server to serve the Swagger spec source.
jensolegSwaggerSchemaUIServer'
    :: Server api -> Server (SwaggerSchemaUI' dir api)
jensolegSwaggerSchemaUIServer' =
    swaggerSchemaUIServerImpl' jensolegIndexTemplate jensolegFiles

jensolegIndexTemplate :: Text
jensolegIndexTemplate = $(embedText "jensoleg.index.html.tmpl")

jensolegFiles :: [(FilePath, ByteString)]
jensolegFiles = $(embedRecursiveDir "jensoleg-dist")
