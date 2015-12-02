{-# LANGUAGE OverloadedStrings #-}
-- | This module adds support for some of PostgreSQL operators on IP addresses
--   and networks. See <http://www.postgresql.org/docs/9.4/static/functions-net.html>
--   for more detailed documentation
module Database.Persist.IP ( (<:<.)
                           , (>:>.)
                           , (<:<=)
                           , (>:>=)
                           , (<.<.)
                           , (>.>.)
                           , IP
                           , IPRange
                           ) where

import Data.IP
import Database.Persist
import Database.Persist.Instances.IP ()
import Unsafe.Coerce (unsafeCoerce)

-- | The record range is contained within the specified range. Corresponds to PgSQL operator <<.
(<:<.) :: EntityField record IPRange -> IPRange -> Filter record
field <:<. range = Filter field (Left range) (BackendSpecificFilter "<<")

-- | The record range contains the specified range. Corresponds to PgSQL operator >>.
(>:>.) :: EntityField record IPRange -> IPRange -> Filter record
field >:>. range = Filter field (Left range) (BackendSpecificFilter ">>")

-- | The record range is contained within or equals to the specified range. Corresponds to PgSQL operator <<=.
(<:<=) :: EntityField record IPRange -> IPRange -> Filter record
field <:<= range = Filter field (Left range) (BackendSpecificFilter "<<=")

-- | The record range contains or equals to the specified range. Corresponds to PgSQL operator >>=.
(>:>=) :: EntityField record IPRange -> IPRange -> Filter record
field >:>= range = Filter field (Left range) (BackendSpecificFilter ">>=")

-- | The record address is contained within the specified range. Corresponds to PgSQL operator <<.
(<.<.) :: EntityField record IP -> IPRange -> Filter record
field <.<. range = Filter (unsafeCoerce field :: EntityField record IPRange) (Left range) (BackendSpecificFilter "<<")

-- | The record range contains the specified address. Corresponds to PgSQL operator >>.
(>.>.) :: EntityField record IPRange -> IP -> Filter record
field >.>. ip = Filter (unsafeCoerce field :: EntityField record IP) (Left ip) (BackendSpecificFilter ">>")
