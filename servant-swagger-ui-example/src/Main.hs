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

import Prelude ()
import Prelude.Compat

import Control.Lens       hiding ((.=))
import Data.Aeson         (FromJSON, ToJSON)
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
import Servant.Swagger.UI.Core
import Servant.Swagger.UI.JensOleG
import Servant.Swagger.UI.ReDoc

import qualified Network.Wai.Handler.Warp as Warp

#if MIN_VERSION_servant(0,12,0)
#define SUMMARY(d) Summary d :>
#else
#define SUMMARY(d)
#endif

#if MIN_VERSION_servant(0,13,0)
#if __GLASGOW_HASKELL__ >= 802 && MIN_VERSION_base(4,10,0)
import GHC.Generics (D1, Meta (..), Rep)
import GHC.TypeLits (AppendSymbol, Symbol)
#endif
#endif

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
instance FromJSON Cat
instance FromJSON CatName
instance ToParamSchema CatName
instance ToSchema Cat
instance ToSchema CatName

-- api

#if MIN_VERSION_servant(0,13,0)
#if __GLASGOW_HASKELL__ >= 802 && MIN_VERSION_base(4,10,0)
-- | Get a typename as type-level 'Symbol'.
--
-- Few non-Generic things are hard-coded, for else we fallback to 'Generic'
--
-- >>> :kind! TypeName Int
-- TypeName Int :: Symbol
-- = "Int"
--
-- >>> :kind! TypeName CatName
-- TypeName CatName :: Symbol
-- = "CatName"
--
-- Unfortunately we cannot use 'TypeError' for other cases,
-- as 'Rep' isn't reduced so 'GenericTypeName' cannot fall past first
-- equation. So you will get somewhat obscure errors saying
-- "no instance for (KnownSymbol ... (GenericTypeName YourNonGenericType ..."
--
-- >>> :kind! TypeName Float
-- TypeName Float :: Symbol
-- = GenericTypeName Float (Rep Float ())
--
-- It would be nice if there were such type family for 'Typeable', i.e. all
-- types :)
--
type family TypeName (x :: *) :: Symbol where
    TypeName Int  = "Int"
    TypeName Text = "Text"
    TypeName x    = GenericTypeName x (Rep x ())

type family GenericTypeName t (r :: *) :: Symbol where
    GenericTypeName t (D1 ('MetaData name mod pkg nt) f x) = name

type Desc t n = Description (AppendSymbol (TypeName t) (AppendSymbol " | " n))
#else
type Desc t n = Description n
#endif
#endif

type FirstCatEndpoint =
#if MIN_VERSION_servant(0,13,0)
    "cat"
        :> Summary "First cat endpoint"
        :> Capture' '[Desc CatName "Cat's name"] ":name" CatName
        :> QueryParam' '[Required, Desc Int "Random number"] "num" Int
        :> QueryParam' '[Optional, Desc Text "Random text"] "text" Text
        :> Get '[JSON] Cat
#else
    "cat" :> Capture ":name" CatName
        :> QueryParam "num" Int :> QueryParam "text" Text :> Get '[JSON] Cat
#endif

type BasicAPI = Get '[PlainText, JSON] Text
    :<|> FirstCatEndpoint
    :<|> SUMMARY("Second cat") "cat2" :> Capture ":name" CatName :> Get '[JSON] Cat
    :<|> SUMMARY("Third cat") "cat3" :> Capture ":name" CatName :> Get '[JSON] Cat
    :<|> SUMMARY("Post endpoint") "post-cat" :> ReqBody '[JSON] Cat :> Post '[JSON] Cat

type API =
    -- this serves both: swagger.json and swagger-ui
    SwaggerSchemaUI "swagger-ui" "swagger.json" Swagger
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

data UIFlavour
    = Original
    | JensOleG
    | ReDoc
    deriving (Eq)

server' :: UIFlavour -> Server API'
server' uiFlavour = server Normal
    :<|> server Nested
    :<|> schemaUiServer (swaggerDoc' SpecDown)
  where
    server :: Variant -> Server API
    server variant =
        schemaUiServer (swaggerDoc' variant)
        :<|> (return "Hello World" :<|> catEndpoint' :<|> catEndpoint :<|> catEndpoint :<|> return)
      where
        catEndpoint' n _ _ = return $ Cat n (variant == Normal)
        catEndpoint  n     = return $ Cat n (variant == Normal)
        -- Unfortunately we have to specify the basePath manually atm.

    schemaUiServer
        :: (Server api ~ Handler Swagger)
        => Swagger -> Server (SwaggerSchemaUI' dir api)
    schemaUiServer = case uiFlavour of
        Original -> swaggerSchemaUIServer
        JensOleG -> jensolegSwaggerSchemaUIServer
        ReDoc    -> redocSchemaUIServer

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

app :: UIFlavour -> Application
app = serve api . server'

main :: IO ()
main = do
    args <- getArgs
    let uiFlavour | "jensoleg" `elem` args = JensOleG
                  | "redoc"    `elem` args = ReDoc
                  | otherwise              = Original
    p <- fromMaybe 8000 . (>>= readMaybe) <$> lookupEnv "PORT"
    putStrLn $ "http://localhost:" ++ show p ++ "/"
    Warp.run p (app uiFlavour)
