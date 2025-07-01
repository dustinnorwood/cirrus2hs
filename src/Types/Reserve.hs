{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Types.Reserve
    ( generateReserveFileString
    ) where

import           Control.Applicative ((<|>))
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

data Reserve = Reserve
  { address :: Text
  , assetRootAddress :: Text
  , cataAPYRate :: Integer
  , cataToken :: Text
  , isActive :: Bool
  , lastUpdatedOraclePrice :: Double
  , loanToValueRatio :: Integer
  , name :: Text
  , oracle :: Text
  , owner :: Text
  , priceOfCATA :: Double
  , unitConversionRate :: Integer
  , liquidationRatio :: Integer
  , usdstToken :: Text
  , burnerAddress :: Text
  , stratstoUSDSTFactor :: Integer
  , usdstPrice :: Integer
  } deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

fileHeader :: String
fileHeader = unlines
  [ "{-# LANGUAGE OverloadedStrings #-}"
  , ""
  , "module Blockchain.GenesisBlocks.Instances.GenesisReserves where"
  , ""
  , "import           Blockchain.Strato.Model.Address"
  , "import           Data.Text                       (Text)"
  , ""
  , "data Reserve = Reserve"
  , "  { address                :: Address"
  , "  , assetRootAddress       :: Address"
  , "  , cataAPYRate            :: Integer"
  , "  , cataToken              :: Address"
  , "  , isActive               :: Bool"
  , "  , lastUpdatedOraclePrice :: Double"
  , "  , loanToValueRatio       :: Integer"
  , "  , name                   :: Text"
  , "  , oracle                 :: Address"
  , "  , owner                  :: Address"
  , "  , priceOfCATA            :: Double"
  , "  , unitConversionRate     :: Integer"
  , "  , liquidationRatio       :: Integer"
  , "  , usdstToken             :: Address"
  , "  , burnerAddress          :: Address"
  , "  , stratstoUSDSTFactor    :: Integer"
  , "  , usdstPrice             :: Integer"
  , "  } deriving (Eq, Show)"
  , ""
  , "reserves :: [Reserve]"
  , "reserves ="
  ]

showAddress :: Text -> String
showAddress a = "0x" ++ T.unpack (head $ T.splitOn ":" a)

showParens :: Show a => a -> String
showParens a = '(' : (show a ++ ")")

printReserve :: Reserve -> String
printReserve Reserve{..} = intercalate " "
  [ "Reserve"
  , showAddress address
  , showAddress assetRootAddress
  , showParens cataAPYRate
  , showAddress cataToken
  , show isActive
  , showParens lastUpdatedOraclePrice
  , showParens loanToValueRatio
  , show name
  , showAddress oracle
  , showAddress owner
  , showParens priceOfCATA
  , showParens unitConversionRate
  , showParens liquidationRatio
  , showAddress usdstToken
  , showAddress burnerAddress
  , showParens stratstoUSDSTFactor
  , showParens usdstPrice
  ]

fileFooter :: String
fileFooter = unlines
  [ "  ]"
  , ""
  ]

generateReserveFileString :: [Value] -> String
generateReserveFileString values =
  let reserveMap = foldr (\v m -> fromMaybe m $ do
        obj <- toObject v
        addr <- obj .!~ "address"
        reserve <- Reserve addr
                       <$> (obj .!~ "assetRootAddress")
                       <*> (obj .!# "cataAPYRate")
                       <*> (obj .!~ "cataToken")
                       <*> (obj .!? "isActive")
                       <*> (obj .!## "lastUpdatedOraclePrice")
                       <*> (obj .!# "loanToValueRatio")
                       <*> (obj .!~ "name")
                       <*> (obj .!~ "oracle")
                       <*> (obj .!~ "owner")
                       <*> (obj .!## "priceOfCATA")
                       <*> (obj .!# "unitConversionRate")
                       <*> (Just $ fromMaybe 0 (obj .!# "liquidationRatio"))
                       <*> ((obj .!~ "usdstToken") <|> (obj .!~ "stratsToken"))
                       <*> (Just $ fromMaybe "0000" (obj .!~ "burnerAddress"))
                       <*> (Just $ fromMaybe 100000000000000 (obj .!# "stratstoUSDSTFactor"))
                       <*> (Just $ fromMaybe 1000000000000000000 (obj .!# "usdstPrice"))
        pure $ M.insert addr reserve m 
        ) M.empty values
      linePrefixes = ("  [ ") : repeat ("  , ")
   in fileHeader
      ++ unlines (zipWith (++) linePrefixes $ printReserve <$> M.elems reserveMap)
      ++ fileFooter