{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings  #-}
module Database.Persist.Instances.IP where

#if __GLASGOW_HASKELL__ <= 708
import Control.Applicative (pure, (<$>))
#endif
import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Data.ByteString.Char8 (pack,unpack)
import Data.IP (IPRange, IP)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Database.Persist
import Database.Persist.Sql
import Text.Read (readMaybe)
import Web.PathPieces (PathPiece(..))

instance PersistField IP where
    toPersistValue = PersistDbSpecific . pack . show

    fromPersistValue (PersistDbSpecific v) = fromMaybe (Left "Unable to parse IP") (pure <$> readMaybe (unpack v))
    fromPersistValue _ = Left "IP must be converted from PersistDbSpecific"

instance PersistFieldSql IP where
    sqlType _ = SqlOther "INET"

instance PersistField IPRange where
    toPersistValue = PersistDbSpecific . pack . show

    fromPersistValue (PersistDbSpecific v) = fromMaybe (Left "Unable to parse IPRange") (pure <$> readMaybe (unpack v))
    fromPersistValue _ = Left "IPRange must be converted from PersistDbSpecific"

instance PersistFieldSql IPRange where
    sqlType _ = SqlOther "CIDR"

instance FromJSON IPRange where
    parseJSON (String s) = maybe (fail "Failed to parse IPRange") pure (readMaybe $ T.unpack s)
    parseJSON v = typeMismatch "IPRange" v

instance ToJSON IPRange where
    toJSON = String . T.pack . show

instance PathPiece IPRange where
    fromPathPiece = readMaybe . T.unpack
    toPathPiece = T.pack . show

instance FromJSON IP where
    parseJSON (String s) = maybe (fail "Failed to parse IP") pure (readMaybe $ T.unpack s)
    parseJSON v = typeMismatch "IP" v

instance ToJSON IP where
    toJSON = String . T.pack . show

instance PathPiece IP where
    fromPathPiece = readMaybe . T.unpack
    toPathPiece = T.pack . show
