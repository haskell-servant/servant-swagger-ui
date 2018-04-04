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
module Servant.Swagger.UI.Internal (mkRecursiveEmbedded, embedText) where

import Prelude ()
import Prelude.Compat

import Control.Arrow                    (first)
import Control.Monad                    (forM)
import Control.Monad.Trans.State.Strict (runState, state)
import Data.Functor.Compose             (Compose (..))
import Data.Int                         (Int64)
import Language.Haskell.TH
import System.Directory
       (doesDirectoryExist, getDirectoryContents)
import System.FilePath                  (makeRelative, (</>))
import System.IO.Unsafe                 (unsafePerformIO)

import qualified Codec.Compression.Lzma  as LZMA
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Lazy    as BSL
import qualified Data.ByteString.Unsafe  as BS.Unsafe
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Encoding as TLE

import Instances.TH.Lift ()

getRecursiveContents :: FilePath -> IO [(FilePath, BSL.ByteString)]
getRecursiveContents topdir = do
  names <- getDirectoryContents topdir
  let properNames = filter (`notElem` [".", ".."]) names
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
    bs = BSL.toStrict $ LZMA.compressWith params lbs
    s = litE $ stringPrimL $ BS.unpack bs
    l = litE $ integerL $ fromIntegral $ BS.length bs

    params = LZMA.defaultCompressParams
        {- doesn't seem to affect much
        { LZMA.compressLevel = LZMA.CompressionLevel9
        , LZMA.compressLevelExtreme = True
        }
        -}

makeEmbeddedEntry :: Name -> (FilePath, (Int64, Int64)) -> Q Exp
makeEmbeddedEntry name (path, (off, len)) =
    [| (path, BSL.toStrict $ BSL.take len $ BSL.drop off $(varE name)) |]

concatEntries :: Traversable t => t BSL.ByteString -> (BSL.ByteString, t (Int64, Int64))
concatEntries xs = (bslEndo BSL.empty, ys)
  where
    (ys, (_, bslEndo)) = runState (traverse (state . single) xs) (0, id)

    single
        :: BSL.ByteString                             -- file bytestring
        -> (Int64, BSL.ByteString -> BSL.ByteString)  -- current offset, buffer so far
        -> ((Int64, Int64), (Int64, BSL.ByteString -> BSL.ByteString))
    single bsl (off, endo) = ((off, l), (off + l, endo . BSL.append bsl))
      where
        l = fromIntegral $ BSL.length bsl

-- | Create a @[('FilePath', 'BSL.ByteString')]@ list, recursively traversing given directory path.
--
-- > staticApp $ embeddedSettings $(mkRecursiveEmbedded "static")
-- > -- is an in-memory equivalent of
-- > staticApp $ defaultFileServerSettings "static"
mkRecursiveEmbedded :: FilePath -> Q Exp
mkRecursiveEmbedded topdir = do
    pairs <- runIO $ fmap (makeAllRelative topdir) $ getRecursiveContents topdir
    -- we do a hop to only embed single big bytestring.
    -- it's beneficial as lzma have more stuff to compress
    let (bsl, Compose offsets) = concatEntries (Compose pairs)
    bslName <- newName "embedBsl"
    bslExpr <- lazyBytestringE bsl
    letE [ return $ ValD (VarP bslName) (NormalB bslExpr) [] ] $
        listE $ map (makeEmbeddedEntry bslName) offsets

-- | Create a textual 'T.Text' from a UTF8-encoded file.
embedText :: FilePath -> Q Exp
embedText fp = do
    bsl <- runIO $ BSL.readFile fp
    case TLE.decodeUtf8' bsl of
        Left e  -> reportError (show e)
        Right _ -> return ()
    [| TL.toStrict $ TLE.decodeUtf8 $ $(lazyBytestringE bsl) |]
