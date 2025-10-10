{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Aeson.Util where

import           Data.Aeson
import           Data.Aeson.Key      (fromText)
import           Data.Aeson.KeyMap   (KeyMap, (!?))
import           Data.Scientific     (coefficient, toRealFloat)
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Text.Read           (readMaybe)

toObject :: Value -> Maybe Object
toObject (Object o) = Just o
toObject _          = Nothing

toArray :: Value -> Maybe Array
toArray (Array a) = Just a
toArray _         = Nothing

toTextObject :: Object -> Maybe (KeyMap Text)
toTextObject = traverse $ \case
  String t -> Just t
  Number s -> Just . T.pack $ show s
  Bool   b -> Just . T.pack $ show b
  Null     -> Just "null"
  _        -> Nothing

(.!~) :: Object -> Text -> Maybe Text
(.!~) o field = o !? fromText field >>= \case
  String t -> pure t
  _ -> Nothing

(.!~~) :: Object -> Text -> Maybe String
(.!~~) o field = o !? fromText field >>= \case
  String t -> pure $ T.unpack t
  _ -> Nothing

(.!#) :: Object -> Text -> Maybe Integer
(.!#) o field = o !? fromText field >>= \case
  String t -> readMaybe $ T.unpack t
  Number s -> pure . round $ toRealFloat s
  _ -> Nothing

(.!##) :: Object -> Text -> Maybe Double
(.!##) o field = o !? fromText field >>= \case
  String t -> readMaybe $ T.unpack t
  Number s -> pure $ toRealFloat s
  _ -> Nothing

(.!?) :: Object -> Text -> Maybe Bool
(.!?) o field = o !? fromText field >>= \case
  String t -> case T.toLower t of
    "true" -> pure True
    "false" -> pure False
    _ -> Nothing
  Bool b -> pure b
  _ -> Nothing