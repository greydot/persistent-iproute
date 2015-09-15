{-# LANGUAGE OverloadedStrings #-}
module Database.Persist.IP ( (<:<.)
                           , (>:>.)
                           , (<.<.)
                           , (>.>.)
                           ) where

import Data.IP
import Database.Persist
import Database.Persist.Instances.IP ()
import Unsafe.Coerce (unsafeCoerce)

(<:<.) :: EntityField record IPRange -> IPRange -> Filter record
field <:<. range = Filter field (Left range) (BackendSpecificFilter "<<")

(>:>.) :: EntityField record IPRange -> IPRange -> Filter record
field >:>. range = Filter field (Left range) (BackendSpecificFilter ">>")

(<.<.) :: EntityField record IP -> IPRange -> Filter record
field <.<. range = Filter (unsafeCoerce field :: EntityField record IPRange) (Left range) (BackendSpecificFilter "<<")

(>.>.) :: EntityField record IPRange -> IP -> Filter record
field >.>. ip = Filter (unsafeCoerce field :: EntityField record IP) (Left ip) (BackendSpecificFilter ">>")
