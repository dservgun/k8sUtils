-- @Author: dinkar
-- @Date:   2020-10-18 09:14:02
-- @Last Modified by:   dinkar
-- @Last Modified time: 2020-10-18 09:29:57

module CommonPredicates where

import CommonTypes
import Data.Functor.Contravariant
import Data.Coerce
import Data.Text as Text
import Data.Text.Encoding as Text

-- DNS protocol limits the length of a label to 63 bytes
isValidDNSLabelName :: Predicate DNSLabelName
isValidDNSLabelName = Predicate (\a -> (Text.length $ coerce a) <= 63)