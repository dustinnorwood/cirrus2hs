{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Types.Crypto
    ( generateCryptoReportCSVs
    ) where

import           Data.Aeson
import           Data.Aeson.Key      (fromText)
import           Data.Aeson.KeyMap   (KeyMap, toMapText, (!?))
import           Data.Aeson.Util
import           Data.Foldable       (toList)
import           Data.Function       (on)
import           Data.List           (intercalate, sortBy)
import qualified Data.Map.Strict     as M
import           Data.Maybe          (catMaybes, fromMaybe)
import           Data.Scientific     (coefficient)
import           GHC.Generics

generateCryptoReportCSVs :: String -> [Value] -> [Value] -> [(String, String)]
generateCryptoReportCSVs reportTimestamp cryptoBalances cryptoTransfers =
  let balanceMap = foldr (\v m -> fromMaybe m $ do
        obj <- toObject v
        n <- obj .!~~ "name"
        cn <- obj .!~~ "ownerCommonName"
        q <- obj .!# "quantity"
        let dec = fromMaybe 18 (obj .!# "decimals")
        pure $ M.alter (\case
            Nothing -> Just (dec, M.singleton cn q)
            Just (dec', m') -> Just (dec', M.alter (\case
              Nothing -> Just q
              Just b -> Just $ b + q
              ) cn m')
          ) n m
        ) M.empty cryptoBalances
      transfersMap = M.map (sortBy ((flip compare) `on` (\(a,_,_,_) -> a))) $ foldr (\v m -> fromMaybe m $ do
        obj <- toObject v
        n <- obj .!~~ "name"
        let f a = do
              buyer <- a .!~~ "purchaserCommonName"
              seller <- a .!~~ "sellerCommonName"
              bts <- a .!~~ "block_timestamp"
              minNum <- a .!# "minItemNumber"
              maxNum <- a .!# "maxItemNumber"
              let q = maxNum - minNum + 1
              pure (bts, seller, buyer, q)
        transfers <- catMaybes . map f . toList <$> (traverse toObject =<< toArray =<< obj !? fromText "BlockApps-Mercata-Asset-OwnershipTransfer")
        pure $ M.insert n transfers m
        ) M.empty cryptoTransfers
      fileHeader n = "Asset Name, Owner Common Name, Quantity (units), Quantity (" ++ n ++ ")"
   in M.toList . flip M.mapWithKey balanceMap $ \assetName (dec, balances) ->
        let assetTransfers = fromMaybe [] $ M.lookup assetName transfersMap
            relevantTransfers = takeWhile (\(t,_,_,_) -> t > reportTimestamp) assetTransfers
            balances' = M.toList $ foldr (\(_,o,n,q) m -> 
                M.alter (Just . maybe q (+q)) o $ M.alter (Just . maybe (-q) (\i -> i-q)) n m
              ) balances relevantTransfers
         in unlines . (fileHeader assetName:) $ (\(cn, b) -> intercalate ", " [assetName, cn, show b, show $ foldr (const (/10)) (fromIntegral b) [1..dec]]) <$> balances'