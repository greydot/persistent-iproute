{-# LANGUAGE OverloadedStrings  #-}
module Database.Persist.Instances.IP where

import Data.ByteString.Char8 (pack,unpack)
import Data.IP
import Data.Maybe (fromMaybe)
import Database.Persist
import Database.Persist.Sql
import Text.Read (readMaybe)

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
