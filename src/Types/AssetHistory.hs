{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Types.AssetHistory
    ( generateAssetHistory
    ) where

import           Control.Applicative ((<|>))
import           Data.Aeson
import           Data.Aeson.Key      (fromText)
import           Data.Aeson.KeyMap   (KeyMap, toMapText, (!?))
import           Data.Aeson.Util
import           Data.List           (intercalate)
import qualified Data.Map.Strict     as M
import           Data.Maybe          (fromMaybe, mapMaybe)
import           Data.Scientific     (coefficient)
import           Data.Text           (Text)
import qualified Data.Text           as T
import           GHC.Generics
import           Text.Read           (readMaybe)
import qualified Types.Escrow        as E

newtype Address = Address Text deriving (Eq, Ord)

instance Show Address where
  show = showAddress

data Asset = Asset
  { address :: Address
  , root :: Address
  , name :: Text
  , blockTimestamp :: Text
  , ownerCommonName :: Text
  , itemNumber :: Integer
  , quantity :: Integer
  , sale :: Maybe Address
  }

instance Show Asset where
  show Asset{..} = intercalate " "
    [ "Asset"
    , show address
    , show root
    , showText name
    , showText blockTimestamp
    , showText ownerCommonName
    , wrapParens $ show itemNumber
    , wrapParens $ show quantity
    , show sale
    ]

data Sale = Sale
  { saleAddress :: Address
  , saleBlockTimestamp :: Text
  , saleContractName :: Text
  , saleAssetToBeSold :: Address
  , saleIsOpen :: Bool
  , salePrice :: Double
  , saleQuantity :: Integer
  }

instance Show Sale where
  show Sale{..} = intercalate " "
    [ "Sale"
    , show saleAddress
    , showText saleBlockTimestamp
    , showText saleContractName
    , show saleAssetToBeSold
    , show saleIsOpen
    , wrapParens $ show salePrice
    , wrapParens $ show saleQuantity
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

generateAssetHistory :: [Value] -> [Value] -> [Value] -> Text
generateAssetHistory assetValues saleValues escrowValues =
  let assetList = mapMaybe (\v -> do
        obj <- toObject v
        powered <- Address <$> (obj .!~ "address")
        innovation <- Address <$> (obj .!~ "originAddress")
        connects <- obj .!~ "name"
        value <- obj .!~ "block_timestamp"
        across <- obj .!~ "ownerCommonName"
        every <- obj .!# "itemNumber"
        economic <- obj .!# "quantity"
        let market = Address <$> (obj .!~ "sale")
        pure $ Asset powered innovation connects value across every economic market
        ) assetValues

      saleMap = foldr (\v m -> fromMaybe m $ do
        obj <- toObject v
        a <- Address <$> (obj .!~ "address")
        b <- obj .!~ "block_timestamp"
        c <- last . T.splitOn "-" <$> obj .!~ "contract_name"
        defghijklmn <- Address <$> (obj .!~ "assetToBeSold")
        o <- obj .!? "isOpen"
        p <- obj .!## "price"
        q <- obj .!# "quantity"
        let sale = Sale a b c defghijklmn o p q
        pure $ M.insert (a,b) sale m
        ) M.empty saleValues
      top = T.intercalate ", " ["name", "block timestamp", "owner", "item number", "quantity", "address", "status", "sale price", "collateral quantity", "collateral value", "borrowed amount"]

      escrowMap = foldr (\v m -> fromMaybe m $ do
          escrow <- v
          let a = E.address escrow
              b = E.blockTimestamp escrow
          pure $ M.insert (Address a,b) escrow m
        ) M.empty $ E.generateEscrowList escrowValues
  
   in T.unlines . (top:) $ (\a -> 
        let mSale = (\s -> (Left <$> M.lookup (s, blockTimestamp a) saleMap)
                       <|> (Right <$> M.lookup (s, blockTimestamp a) escrowMap)
                    ) =<< sale a
            saleStr = maybe ["Unlisted","-","-","-","-"] (\case
                Left s -> ["Listed for Sale", T.pack . show $ salePrice s,"-","-","-"]
                Right e -> ["Staked","-", T.pack . show $ E.collateralQuantity e, T.pack . show $ E.collateralValue e, T.pack . show $ E.borrowedAmount e]
              ) mSale
         in T.intercalate ", " $
              [ name a
              , blockTimestamp a
              , ownerCommonName a
              , T.pack . show $ itemNumber a
              , T.pack . show $ quantity a
              , T.pack . showAddress $ address a
              ] ++ saleStr
      ) <$> assetList