{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main (main, catIsMale, catName) where

import Control.Lens       hiding ((.=))
import Data.Aeson         (ToJSON)
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

-- data types
data Cat = Cat { catName :: CatName, catIsMale :: Bool }
    deriving (Generic, Show)

newtype CatName = CatName Text
    deriving ( Eq, Show, Generic
#if MIN_VERSION_servant(0,5,0)
             , FromHttpApiData
#else
             , FromText
#endif
             )

instance IsString CatName where
    fromString = CatName . fromString

-- swagger instances
instance ToJSON Cat
instance ToJSON CatName
instance ToParamSchema CatName
instance ToSchema Cat
instance ToSchema CatName

-- api
type BasicAPI = Get '[PlainText, JSON] Text
    :<|> "cat" :> Capture ":name" CatName :> Get '[JSON] Cat
    :<|> "cat2" :> Capture ":name" CatName :> Get '[JSON] Cat
    :<|> "cat3" :> Capture ":name" CatName :> Get '[JSON] Cat

type API =
    -- this serves both: swagger.json and swagger-ui
    SwaggerSchemaUI "swagger-ui" "swagger.json"
    :<|> BasicAPI

-- To test nested case
type API' = API
    :<|> "nested" :> API
    :<|> SwaggerSchemaUI' "foo-ui" ("foo" :> "swagger.json" :> Get '[JSON] Swagger)

-- Implementation

-- | We test different ways to nest API, so we have an enumeration
data Variant
    = Normal
    | Nested
    | SpecDown
    deriving (Eq)

server' :: Server API'
server' = server Normal
    :<|> server Nested
    :<|> swaggerSchemaUIServer (swaggerDoc' SpecDown)
  where
    server :: Variant -> Server API
    server variant =
        swaggerSchemaUIServer (swaggerDoc' variant)
        :<|> (return "Hello World" :<|> catEndpoint :<|> catEndpoint :<|> catEndpoint)
      where
        catEndpoint n = return $ Cat n (variant == Normal)
        -- Unfortunately we have to specify the basePath manually atm.

    swaggerDoc' Normal    = swaggerDoc
    swaggerDoc' Nested    = swaggerDoc
        & basePath ?~ "/nested"
        & info.description ?~ "Nested API"
    swaggerDoc' SpecDown  = swaggerDoc
        & info.description ?~ "Spec nested"

-- Boilerplate

swaggerDoc :: Swagger
swaggerDoc = toSwagger (Proxy :: Proxy BasicAPI)
    & info.title       .~ "Cats API"
    & info.version     .~ "2016.8.7"
    & info.description ?~ "This is an API that tests servant-swagger support"

api :: Proxy API'
api = Proxy

app :: Application
app = serve api server'

main :: IO ()
main = do
    args <- getArgs
    case args of
        ("run":_) -> do
            p <- fmap (fromMaybe 8000 . (>>= readMaybe)) $ lookupEnv "PORT"
            Warp.run p app
        _ -> putStrLn "To run the example server, pass 'run' argument"
