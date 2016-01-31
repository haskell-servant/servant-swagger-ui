{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
------------------------------------------------------------
-- |
-- Module      :  Servant.Swagger.UI.Internal
-- Copyright   :  (c) 2015 Futurice
-- License     :  MIT
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- Originally from waitra package:
-- <http://hackage.haskell.org/package/waitra-0.0.4.0/docs/Network-Waitra-Embedded.html>
----------------------------------------------------------------------------
module Servant.Swagger.UI.Internal (mkRecursiveEmbedded) where

import           Control.Arrow         (first)
import           Control.Monad         (forM)
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy  as BL
import           Language.Haskell.TH
import           System.Directory      (doesDirectoryExist,
                                        getDirectoryContents)
import           System.FilePath       (makeRelative, (</>))

getRecursiveContents :: FilePath -> IO [(FilePath, BL.ByteString)]
getRecursiveContents topdir = do
  names <- getDirectoryContents topdir
  let properNames = Prelude.filter (`notElem` [".", ".."]) names
  paths <- forM properNames $ \name -> do
    let path = topdir </> name
    isDirectory <- doesDirectoryExist path
    if isDirectory
      then getRecursiveContents path
      else do contents <- BL.readFile path
              return [(path, contents)]
  return (concat paths)

makeAllRelative :: FilePath -> [(FilePath, a)] -> [(FilePath, a)]
makeAllRelative topdir = map (first (("/" ++) . makeRelative topdir))

bytestringE :: B.ByteString -> Q Exp
bytestringE b = [| B8.pack $s |]
  where s = litE $ stringL $ B8.unpack b

makeEmbeddedEntry :: (FilePath, BL.ByteString) -> Q Exp
makeEmbeddedEntry (path, bs) = [| (path, $(bytestringE $ BL.toStrict bs)) |]

-- | Create a @[('FilePath', 'BL.ByteString')]@ list, recursively traversing given directory path.
--
-- > staticApp $ embeddedSettings $(mkRecursiveEmbedded "static")
-- > -- is an in-memory equivalent of
-- > staticApp $ defaultFileServerSettings "static"
mkRecursiveEmbedded :: FilePath -> Q Exp
mkRecursiveEmbedded topdir = do
  pairs <- runIO $ makeAllRelative topdir <$> getRecursiveContents topdir
  listE $ map makeEmbeddedEntry pairs
