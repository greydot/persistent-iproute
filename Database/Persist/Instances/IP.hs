{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings  #-}
module Database.Persist.Instances.IP where

#if __GLASGOW_HASKELL__ <= 708
import Control.Applicative (pure, (<$>))
#endif
import Data.Aeson.IP ()
import Data.ByteString.Char8 (pack,unpack)
import Data.IP (IPRange, IP)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Database.Persist
import Database.Persist.Sql
import Text.Read (readMaybe)
import Web.HttpApiData (ToHttpApiData(..),FromHttpApiData(..))
import Web.PathPieces (PathPiece(..))

instance PersistField IP where
    toPersistValue = PersistDbSpecific . pack . show

    fromPersistValue (PersistDbSpecific v) = fromMaybe (Left "Unable to parse IP") (pure <$> readMaybe (unpack v))
    fromPersistValue _ = Left "IP must be converted from PersistDbSpecific"

instance PersistFieldSql IP where
#ifdef USE_IP4R
    sqlType _ = SqlOther "IPADDRESS"
#else
    sqlType _ = SqlOther "INET"
#endif

instance PersistField IPRange where
    toPersistValue = PersistDbSpecific . pack . show

    fromPersistValue (PersistDbSpecific v) = fromMaybe (Left "Unable to parse IPRange") (pure <$> readMaybe (unpack v))
    fromPersistValue _ = Left "IPRange must be converted from PersistDbSpecific"

instance PersistFieldSql IPRange where
#ifdef USE_IP4R
    sqlType _ = SqlOther "IPRANGE"
#else
    sqlType _ = SqlOther "CIDR"
#endif


-- The following instances don't really make sense, but persistent
-- requires them so I defined them anyway.
instance PathPiece IPRange where
    fromPathPiece = readMaybe . T.unpack . T.replace "%2F" "/"
    toPathPiece = T.replace "/" "%2F" . T.pack . show

instance PathPiece IP where
    fromPathPiece = readMaybe . T.unpack
    toPathPiece = T.pack . show

instance ToHttpApiData IP where
    toUrlPiece = T.pack . show

instance ToHttpApiData IPRange where
    toUrlPiece = T.replace "/" "%2F" . T.pack . show

instance FromHttpApiData IP where
    parseUrlPiece txt
        | Just ip <- readMaybe $ T.unpack txt = Right ip
        | otherwise = Left "Unable to parse IP"

instance FromHttpApiData IPRange where
    parseUrlPiece txt
        | Just ipr <- readMaybe . T.unpack $ T.replace "%2F" "/" txt = Right ipr
        | otherwise = Left "Unable to parse IPRange"
