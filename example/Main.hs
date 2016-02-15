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
    toJSON (CatName n) = toJSON n

instance IsString CatName where
    fromString = CatName . fromString

instance ToParamSchema CatName
instance ToSchema Cat
instance ToSchema CatName

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
    (return "Hello World" :<|> catEndpoint)
    :<|> return swaggerDoc
    :<|> swaggerUIServer
  where
    catEndpoint n = return $ Cat n False

-- Boilerplate

swaggerDoc :: Swagger
swaggerDoc = toSwagger (Proxy :: Proxy BasicAPI)
    & info.title       .~ "Cats API"
    & info.version     .~ "2016.2.6"
    & info.description ?~ "This is an API that tests servant-swagger support "

api :: Proxy API
api = Proxy

app :: Application
app = serve api server

main :: IO ()
main = do
    args <- getArgs
    case args of
        ("run":_) -> do
            p <- fmap (fromMaybe 8000 . (>>= readMaybe)) $ lookupEnv "PORT"
            Warp.run p app
        _ -> putStrLn "To run, pass 'run' argument"
