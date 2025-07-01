{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lib
    ( someFunc
    ) where

import           Data.Aeson
import           Data.Aeson.Key      (fromText)
import           Data.Aeson.KeyMap   (KeyMap, toMapText, (!?))
import           Data.List           (intercalate)
import qualified Data.Map.Strict     as M
import           Data.Maybe          (fromJust)
import           Data.Scientific     (coefficient)
import           Data.Text           (Text)
import qualified Data.Text           as T
import           GHC.Generics
import           Text.Read           (readMaybe)
import           Types.Asset
import           Types.Escrow
import           Types.Reserve

someFunc :: IO ()
someFunc = do
  writeFile "app/files/hs/GenesisAssets.hs" . generateAssetFileString . fromJust =<< decodeFileStrict "app/files/json/assets.json"
  writeFile "app/files/hs/GenesisEscrows.hs" . generateEscrowFileString . fromJust =<< decodeFileStrict "app/files/json/escrows.json"
  writeFile "app/files/hs/GenesisReserves.hs" . generateReserveFileString . fromJust =<< decodeFileStrict "app/files/json/reserves.json"