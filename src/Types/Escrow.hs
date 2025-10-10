{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Types.Escrow
    ( Escrow(..)
    , generateEscrowList
    , generateEscrowFileString
    ) where

import           Control.Monad       ((<=<))
import           Data.Aeson
import           Data.Aeson.Key      (fromText)
import           Data.Aeson.KeyMap   (KeyMap, toMapText, (!?))
import           Data.Aeson.Util
import           Data.List           (intercalate)
import qualified Data.Map.Strict     as M
import           Data.Maybe          (fromMaybe)
import           Data.Scientific     (coefficient)
import           Data.Text           (Text)
import qualified Data.Text           as T
import           GHC.Generics
import           Text.Read           (readMaybe)

data Escrow = Escrow
  { address :: Text
  , assetRootAddress :: Text
  , blockTimestamp :: Text
  , borrowedAmount :: Integer
  , borrower :: Text
  , borrowerCommonName :: Text
  , collateralQuantity :: Integer
  , collateralValue :: Integer
  , isActive :: Bool
  , lastRewardTimestamp :: Integer
  , maxLoanAmount :: Integer
  , reserve :: Text
  , totalCataReward :: Integer
  , liquidationAmount :: Integer
  , version :: Text
  , assets :: M.Map Integer Text
  } deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

fileHeader :: String
fileHeader = unlines
  [ "{-# LANGUAGE OverloadedStrings #-}"
  , ""
  , "module Blockchain.GenesisBlocks.Instances.GenesisEscrows where"
  , ""
  , "import           Blockchain.Strato.Model.Address"
  , "import qualified Data.Map.Strict                 as M"
  , "import           Data.Text                       (Text)"
  , ""
  , "data Escrow = Escrow"
  , "  { address :: Address"
  , "  , assetRootAddress :: Address"
  , "  , borrowedAmount :: Integer"
  , "  , borrower :: Address"
  , "  , borrowerCommonName :: Text"
  , "  , collateralQuantity :: Integer"
  , "  , collateralValue :: Integer"
  , "  , isActive :: Bool"
  , "  , lastRewardTimestamp :: Integer"
  , "  , maxLoanAmount :: Integer"
  , "  , reserve :: Address"
  , "  , totalCataReward :: Integer"
  , "  , liquidationAmount :: Integer"
  , "  , version :: Text"
  , "  , assets :: M.Map Integer Address"
  , "  } deriving (Eq, Show)"
  , ""
  , "escrows :: [Escrow]"
  , "escrows ="
  ]

showAddress :: Text -> String
showAddress a = "0x" ++ T.unpack (head $ T.splitOn ":" a)

showParens :: Show a => a -> String
showParens a = '(' : (show a ++ ")")

showAssets :: M.Map Integer Text -> String
showAssets m = "(M.fromList ["
  ++ intercalate "," ((\(k, v) -> "(" ++ showParens k ++ ", " ++ showAddress v ++ ")") <$> M.toList m)
  ++ "])"

printEscrow :: Escrow -> String
printEscrow Escrow{..} = intercalate " "
  [ "Escrow"
  , showAddress address
  , showAddress assetRootAddress
  , showParens borrowedAmount
  , showAddress borrower
  , show borrowerCommonName
  , showParens collateralQuantity
  , showParens collateralValue
  , show isActive
  , showParens lastRewardTimestamp
  , showParens maxLoanAmount
  , showAddress reserve
  , showParens totalCataReward
  , showParens liquidationAmount
  , show version
  , showAssets assets
  ]

fileFooter :: String
fileFooter = unlines
  [ "  ]"
  , ""
  ]

generateEscrowList :: [Value] -> [Maybe Escrow]
generateEscrowList values =
  map (\v -> do
        obj <- toObject v
        addr <- obj .!~ "address"
        let f a b = fromMaybe b $ do
              k <- readMaybe . T.unpack =<< a .!~ "key"
              v <- a .!~ "value"
              pure $ M.insert k v b
        let assets = foldr f M.empty <$> (traverse toObject =<< toArray =<< obj !? fromText "BlockApps-Mercata-Escrow-assets")
        Escrow addr
                     <$> (obj .!~ "assetRootAddress")
                     <*> (obj .!~ "block_timestamp")
                     <*> (obj .!# "borrowedAmount")
                     <*> (obj .!~ "borrower")
                     <*> (obj .!~ "borrowerCommonName")
                     <*> (obj .!# "collateralQuantity")
                     <*> (obj .!# "collateralValue")
                     <*> (obj .!? "isActive")
                     <*> (obj .!# "lastRewardTimestamp")
                     <*> (obj .!# "maxLoanAmount")
                     <*> (obj .!~ "reserve")
                     <*> (obj .!# "totalCataReward")
                     <*> (Just $ fromMaybe 0 (obj .!# "liquidationAmount"))
                     <*> (Just $ fromMaybe "" (obj .!~ "version"))
                     <*> pure (fromMaybe M.empty assets)
        ) values

generateEscrowFileString :: [Value] -> String
generateEscrowFileString values =
  let escrowMap = foldr (\v m -> fromMaybe m $ do
          escrow <- v
          pure $ M.insert (address escrow) escrow m
        ) M.empty $ generateEscrowList values
      linePrefixes = ("  [ ") : repeat ("  , ")
   in fileHeader
      ++ unlines (zipWith (++) linePrefixes $ printEscrow <$> M.elems escrowMap)
      ++ fileFooter