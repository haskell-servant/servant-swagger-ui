#!/usr/bin/env runghc
{-# OPTIONS_GHC -Wall #-}

import Data.Char (toLower)
import Data.Foldable (traverse_)
import Data.List (isPrefixOf)
import Data.Version (Version, parseVersion)
import System.Environment (getArgs)
import Text.ParserCombinators.ReadP (readP_to_S)

-------------------------------------------------------------------------------
-- Definitions
-------------------------------------------------------------------------------

-- | The guts of the implementation works with this value.
defns :: [Defn]
defns = map defn [minBound .. maxBound]

-- | C is a helper type to make 'defns'
data C
    = Servant_0_4
    | Servant_0_5
    | Servant_0_6
    | Servant_0_7
    | Servant_0_8
    | Servant_0_9
    | Servant_0_10
    | Servant_0_11
    | Servant_0_12
  deriving (Eq, Show, Enum, Bounded)

-- Constraint definitions
defn :: C -> Defn
defn c = case c of
    -- identifier, constraint, min <= v < max ghc versions
    -- Note: we shouldn't have spaces in the constraint, because /bin/sh
    Servant_0_4  -> mkDefn c ["servant==0.4.4.*"]  "7.8" "8.0"
    Servant_0_5  -> mkDefn c ["servant==0.5.*"  ]  "7.8" "8.0"
    Servant_0_6  -> mkDefn c ["servant==0.6.*"  ]  "7.8" "8.0"
    Servant_0_7  -> mkDefn c ["servant==0.7.*"  ]  "7.8" "8.2"
    Servant_0_8  -> mkDefn c ["servant==0.8.*"  ]  "7.8" "8.2"
    Servant_0_9  -> mkDefn c ["servant==0.9.*"  ]  "7.8" "8.2"
    Servant_0_10 -> mkDefn c ["servant==0.10.*" ]  "7.8" "8.2"
    Servant_0_11 -> mkDefn c ["servant==0.11.*" ]  "7.8" "8.3"
    Servant_0_12 -> mkDefn c ["servant==0.12.*" ]  "7.8" "8.3"

mkDefn :: C -> [String] -> String -> String -> Defn
mkDefn c x mi ma = Defn (map toLower $ show c) x (unsafeMkV mi) (unsafeMkV ma)

-------------------------------------------------------------------------------
-- Implementation
-------------------------------------------------------------------------------

data Defn = Defn
    { defnId          :: String
    , defnConstraints :: [String]
    , defnMinGHC      :: Version
    , defnMaxGHC      :: Version
    }

mkV :: String -> Maybe Version
mkV s = case filter (null . snd) $ readP_to_S parseVersion s of
    ((v, "") : _) -> Just v
    _             -> Nothing

unsafeMkV :: String -> Version
unsafeMkV s = case mkV s of
    Nothing -> error $ "panic: Cannot parse version: " ++ s
    Just v  -> v

parseGHCVersion :: String -> Maybe Version
parseGHCVersion s
    | "ghc-" `isPrefixOf` s = mkV (drop 4 s)
    | otherwise             = Nothing

unsafeParseGHCVersion :: String -> Version
unsafeParseGHCVersion s = case parseGHCVersion s of
    Nothing -> error $ "panic: Cannot parse ghc version: " ++ s
    Just v  -> v

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["list", ghc] -> traverse_ putStrLn
            [ i
            | Defn i _ mi ma <- defns
            , mi <= v, v < ma
            ]
          where
            v = unsafeParseGHCVersion ghc

        ["constraints", i'] -> traverse_ putStrLn
            [ "--constraint " ++ x
            | Defn i xs _ _ <- defns
            , i' == i
            , x <- xs
            ]

        _ -> error $ "unknown args: " ++ show args

