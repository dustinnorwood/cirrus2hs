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
import           Data.Foldable       (traverse_)
import           Data.List           (intercalate)
import qualified Data.Map.Strict     as M
import           Data.Maybe          (fromJust)
import           Data.Scientific     (coefficient)
import           Data.Text           (Text)
import qualified Data.Text           as T
import           GHC.Generics
import           Text.Read           (readMaybe)
import           Types.Asset
import           Types.AssetHistory
import           Types.Crypto
import           Types.Escrow
import           Types.Reserve

someFunc :: String -> IO ()
someFunc date = do
  writeFile "app/files/hs/GenesisAssets.hs" . generateAssetFileString . fromJust =<< decodeFileStrict "app/files/json/assets.json"
  writeFile "app/files/hs/GenesisEscrows.hs" . generateEscrowFileString . fromJust =<< decodeFileStrict "app/files/json/escrows.json"
  writeFile "app/files/hs/GenesisReserves.hs" . generateReserveFileString . fromJust =<< decodeFileStrict "app/files/json/reserves.json"
  cryptoBalances <- fromJust <$> decodeFileStrict "app/files/json/crypto_balances.json"
  cryptoTransfers <- fromJust <$> decodeFileStrict "app/files/json/crypto_transfers.json"
  traverse_ (\(n,c) -> writeFile ("app/files/csv/CryptoReport_" ++ n ++ "_" ++ date ++ ".csv") c) $ generateCryptoReportCSVs date cryptoBalances cryptoTransfers

  assetHistory <- fromJust <$> decodeFileStrict "app/files/json/asset_history.json"
  saleHistory  <- fromJust <$> decodeFileStrict "app/files/json/sale_history.json"
  escrowHistory  <- fromJust <$> decodeFileStrict "app/files/json/escrow_history.json"
  writeFile "app/files/csv/asset_history.csv" . T.unpack $ generateAssetHistory assetHistory saleHistory escrowHistory