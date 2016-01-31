{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main (main, catIsMale, catName) where

import Control.Lens       hiding ((.=))
import Data.Aeson         (ToJSON (..), object, (.=))
import Data.Maybe         (fromMaybe)
import Data.String        (IsString (..))
import Data.Text          (Text)
import GHC.Generics       (Generic)
import Network.Wai        (Application)
import System.Environment (getArgs, lookupEnv)
import Text.Read          (readMaybe)

import Data.Swagger
import Servant
import Servant.Swagger
import Servant.Swagger.UI

import qualified Network.Wai.Handler.Warp as Warp

--

data Cat = Cat { catName :: CatName, catIsMale :: Bool }
    deriving (Generic, Show)

instance ToJSON Cat where
    toJSON (Cat n g) = object [ "catName" .= n , "catIsMale" .= g]

newtype CatName = CatName Text
    deriving (Eq, Show, Generic, FromText)

instance ToJSON CatName where
    toJSON (CatName name) = toJSON name

instance IsString CatName where
    fromString = CatName . fromString

instance ToParamSchema CatName

instance ToSchema Cat where
  declareNamedSchema proxy = do
    (name, schema) <- genericDeclareNamedSchema defaultSchemaOptions proxy
    return (name, schema
      & schemaDescription ?~ "This is some cat"
      & schemaExample ?~ toJSON (Cat "Felix" True))

instance ToSchema CatName where
  declareNamedSchema proxy = do
    (name, schema) <- genericDeclareNamedSchema defaultSchemaOptions proxy
    return (name, schema
      & schemaDescription ?~ "This is some cat name"
      & schemaExample ?~ toJSON (CatName "Felix"))

---

type BasicAPI = Get '[PlainText, JSON] Text
    :<|> "cat" :> Capture ":name" CatName :> Get '[JSON] Cat

type SwaggerSchemaEndpoint = "swagger.js" :> Get '[JSON] Swagger

data API
type API' = BasicAPI
    :<|> SwaggerSchemaEndpoint
    :<|> SwaggerUI "ui" SwaggerSchemaEndpoint API

instance HasServer API where
  type ServerT API m = ServerT API' m
  route _ = route (Proxy :: Proxy API')

type instance IsElem' e API = IsElem e API'

-- Implementation
server :: Server API
server =
    (pure "Hello World" :<|> catEndpoint)
    :<|> pure swaggerDoc
    :<|> swaggerUIServer
  where
    catEndpoint name = pure $ Cat name False

-- Boilerplate

swaggerDoc :: Swagger
swaggerDoc = toSwagger (Proxy :: Proxy BasicAPI)
    & info.infoTitle   .~ "Cats API"
    & info.infoVersion .~ "2016.1.31"
    & info.infoDescription ?~ "This is an API that tests servant-swagger support "

api :: Proxy API
api = Proxy

app :: Application
app = serve api server

main :: IO ()
main = do
    args <- getArgs
    case args of
        ("run":_) -> do
            port <- fromMaybe 8000 . (>>= readMaybe) <$> lookupEnv "PORT"
            Warp.run port app
        _ -> putStrLn "To run, pass 'run' argument"
