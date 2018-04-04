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

import qualified Codec.Compression.Lzma as LZMA
import           Control.Arrow          (first)
import           Control.Monad          (forM)
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as BSL
import qualified Data.ByteString.Unsafe as BS.Unsafe
import           Language.Haskell.TH
import           System.Directory
                 (doesDirectoryExist, getDirectoryContents)
import           System.FilePath        (makeRelative, (</>))
import           System.IO.Unsafe       (unsafePerformIO)

getRecursiveContents :: FilePath -> IO [(FilePath, BSL.ByteString)]
getRecursiveContents topdir = do
  names <- getDirectoryContents topdir
  let properNames = Prelude.filter (`notElem` [".", ".."]) names
  paths <- forM properNames $ \name -> do
    let path = topdir </> name
    isDirectory <- doesDirectoryExist path
    if isDirectory
      then getRecursiveContents path
      else do contents <- BSL.readFile path
              return [(path, contents)]
  return (concat paths)

makeAllRelative :: FilePath -> [(FilePath, a)] -> [(FilePath, a)]
makeAllRelative topdir = map (first (("/" ++) . makeRelative topdir))

-- | Makes lazy 'BSL.ByteString' expression.
-- Embedded value is compressed with LZMA.
lazyBytestringE :: BSL.ByteString -> Q Exp
lazyBytestringE lbs =
    [| LZMA.decompress
    $ BSL.fromStrict
    $ unsafePerformIO
    $ BS.Unsafe.unsafePackAddressLen $l $s
    |]
  where
    bs = BSL.toStrict $ LZMA.compress lbs
    s = litE $ stringPrimL $ BS.unpack bs
    l = litE $ integerL $ fromIntegral $ BS.length bs

makeEmbeddedEntry :: (FilePath, BSL.ByteString) -> Q Exp
makeEmbeddedEntry (path, bs) = [| (path, BSL.toStrict $(lazyBytestringE bs)) |]

-- | Create a @[('FilePath', 'BSL.ByteString')]@ list, recursively traversing given directory path.
--
-- > staticApp $ embeddedSettings $(mkRecursiveEmbedded "static")
-- > -- is an in-memory equivalent of
-- > staticApp $ defaultFileServerSettings "static"
mkRecursiveEmbedded :: FilePath -> Q Exp
mkRecursiveEmbedded topdir = do
  pairs <- runIO $ fmap (makeAllRelative topdir) $ getRecursiveContents topdir
  listE $ map makeEmbeddedEntry pairs
