{-# LANGUAGE OverloadedStrings #-}
module Database.Persist.IP ( (<:<.)
                           , (>:>.)
                           , module Database.Persist.Instances.IP
                           ) where

import Data.IP
import Database.Persist
import Database.Persist.Instances.IP ()

(<:<.) :: EntityField record PersistValue -> IPRange -> Filter record
field <:<. range = Filter field (Left $ toPersistValue range) (BackendSpecificFilter "<<")

(>:>.) :: EntityField record PersistValue -> IPRange -> Filter record
field >:>. range = Filter field (Left $ toPersistValue range) (BackendSpecificFilter ">>")
