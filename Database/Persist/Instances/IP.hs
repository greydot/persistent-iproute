{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings  #-}
module Database.Persist.Instances.IP where

#if __GLASGOW_HASKELL__ <= 708
import Control.Applicative (pure, (<$>))
#endif
import Data.Aeson.IP ()
import Data.ByteString.Char8 (ByteString, pack,unpack)
import Data.IP (IPRange, IP)
import qualified Data.Text as T
import Database.Persist
import Database.Persist.Sql
import Text.Read (readMaybe)
import Web.HttpApiData (ToHttpApiData(..),FromHttpApiData(..))
import Web.PathPieces (PathPiece(..))

note :: Maybe b -> T.Text -> Either T.Text b
{-# INLINE note #-}
note x y = maybe (Left y) Right x

ctor :: ByteString -> PersistValue
{-# INLINE ctor #-}

ctorName :: T.Text
{-# INLINE ctorName #-}

unCtor :: PersistValue -> Maybe ByteString
{-# INLINE unCtor #-}

#if MIN_VERSION_persistent(2,11,0)
ctor = PersistLiteralEscaped
ctorName = "PersistLiteralEscaped"
unCtor x = case x of {PersistLiteralEscaped y -> Just y; _ -> Nothing}
#else
ctor = PersistDbSpecific
ctorName = "PersistDbSpecific"
unCtor x = case x of {PersistDbSpecific y -> Just y; _ -> Nothing}
#endif

instance PersistField IP where
    toPersistValue = ctor . pack . show

    fromPersistValue pval = do
        ipBS <- unCtor pval `note` mconcat ["IP must be converted from ", ctorName]
        let ipStr = unpack ipBS
        readMaybe ipStr `note` mconcat ["Unable to parse IP: ", T.pack ipStr]

instance PersistFieldSql IP where
#ifdef USE_IP4R
    sqlType _ = SqlOther "IPADDRESS"
#else
    sqlType _ = SqlOther "INET"
#endif

instance PersistField IPRange where
    toPersistValue = ctor . pack . show

    fromPersistValue pval = do
        iprBS <- unCtor pval `note` mconcat ["IPRange must be converted from ", ctorName]
        let iprStr = unpack iprBS
        readMaybe iprStr `note` mconcat ["Unable to parse IPRange: ", T.pack iprStr]

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
        | otherwise = Left $ mconcat ["Unable to parse IP: ", txt]

instance FromHttpApiData IPRange where
    parseUrlPiece txt
        | Just ipr <- readMaybe . T.unpack $ T.replace "%2F" "/" txt = Right ipr
        | otherwise = Left $ mconcat ["Unable to parse IPRange: ", txt]
