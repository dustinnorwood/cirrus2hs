{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Types.Asset
    ( generateAssetFileString
    ) where

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

newtype Address = Address Text deriving (Eq, Ord)

instance Show Address where
  show = showAddress

data Balance = Balance
  { address :: Address
  , owner :: Address
  , ownerCommonName :: Text
  , quantity :: Integer
  }

instance Show Balance where
  show (Balance a o c q) = intercalate " "
    [ "Balance "
    , showAddress a
    , showAddress o
    , showText c
    , wrapParens $ show q
    ]

data Asset = Asset
  { assetType :: Text
  , root :: Address
  , name :: Text
  , description :: Text
  , decimals :: Integer
  , images :: M.Map Integer Text
  , files :: M.Map Integer Text
  , fileNames :: M.Map Integer Text
  , assetData :: M.Map Text Text
  , balances :: M.Map Address Balance
  }

instance Show Asset where
  show Asset{..} = intercalate " "
    [ "Asset"
    , showText assetType
    , show root
    , showText name
    , showText description
    , wrapParens $ show decimals
    , wrapParens $ show images
    , wrapParens $ show files
    , wrapParens $ show fileNames
    , wrapParens $ show assetData
    , wrapParens $ show balances
    ]

fileHeader :: String
fileHeader = unlines
  [ "{-# LANGUAGE OverloadedStrings #-}"
  , ""
  , "module Blockchain.GenesisBlocks.Instances.GenesisAssets where"
  , ""
  , "import Blockchain.Strato.Model.Address"
  , "import Data.Map.Strict (Map, fromList)"
  , "import Data.Text (Text)"
  , ""
  , "data Balance = Balance"
  , "  { address :: Address"
  , "  , owner :: Address"
  , "  , ownerCommonName :: Text"
  , "  , quantity :: Integer"
  , "  } deriving (Eq, Show)"
  , ""
  , "data Asset = Asset"
  , "  { assetType       :: Text"
  , "  , root            :: Address"
  , "  , name            :: Text"
  , "  , description     :: Text"
  , "  , decimals        :: Integer"
  , "  , images          :: Map Integer Text"
  , "  , files           :: Map Integer Text"
  , "  , fileNames       :: Map Integer Text"
  , "  , assetData       :: Map Text Text"
  , "  , balances        :: Map Address Balance"
  , "  } deriving (Eq, Show)"
  , ""
  , "assets :: [Asset]"
  , "assets ="
  ]

showAddress :: Address -> String
showAddress (Address a) = "0x" ++ T.unpack (head $ T.splitOn ":" a)

showText :: Text -> String
showText t = show t

wrapParens :: String -> String
wrapParens i = '(' : i ++ ")"

fileFooter :: String
fileFooter = unlines
  [ "  ]"
  , ""
  ]

generateAssetFileString :: [Value] -> String
generateAssetFileString assetValues =
  let assetMap = foldr (\v m -> fromMaybe m $ do
        obj <- toObject v
        aType <- last . T.splitOn "-" <$> obj .!~ "contract_name"
        root <- Address <$> (obj .!~ "originAddress")
        addr <- Address <$> (obj .!~ "address")
        n <- obj .!~ "name"
        d <- obj .!~ "description"
        i <- obj .!# "itemNumber"
        o <- Address <$> (obj .!~ "owner")
        cn <- obj .!~ "ownerCommonName"
        q <- obj .!# "quantity"
        let dec = fromMaybe 0 (obj .!# "decimals")
            f a b = fromMaybe b $ do
              k <- readMaybe . T.unpack =<< a .!~ "key"
              v <- a .!~ "value"
              pure $ M.insert k v b
        imgs <- foldr f M.empty <$> (traverse toObject =<< toArray =<< obj !? fromText "BlockApps-Mercata-Asset-images")
        files <- foldr f M.empty <$> (traverse toObject =<< toArray =<< obj !? fromText "BlockApps-Mercata-Asset-files")
        fileNames <- foldr f M.empty <$> (traverse toObject =<< toArray =<< obj !? fromText "BlockApps-Mercata-Asset-fileNames")
        ad <- toMapText <$> (toTextObject =<< toObject =<< obj !? fromText "data")
        let bal = Balance addr o cn q
            asset = Asset aType root n d dec imgs files fileNames ad (M.singleton o bal)
        pure $ M.alter (\case
            Nothing -> Just $ M.singleton root asset
            Just assetsByType -> Just $ M.alter (\case
              Nothing -> Just asset
              Just a -> Just $
                if addr == root
                  then
                    asset{balances = M.alter (\case
                      Nothing -> Just bal
                      Just existingBalance -> Just $ existingBalance{
                          address = root,
                          quantity = quantity existingBalance + q
                        }
                      ) o (balances a)}
                  else
                    a{balances = M.alter (\case
                      Nothing -> Just bal
                      Just existingBalance -> Just $ existingBalance{
                          address = address existingBalance,
                          quantity = quantity existingBalance + q
                        }
                      ) o (balances a)}
              ) root assetsByType
            ) aType m
        ) M.empty assetValues
      linePrefixes = ("  [ ") : repeat ("  , ")
   in fileHeader
      ++ unlines (zipWith (++) linePrefixes $ show <$> concat (M.elems <$> M.elems assetMap))
      ++ fileFooter