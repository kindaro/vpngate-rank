module Constants where

import Data.String
import Types

programName :: IsString a => a
programName = "vpngate-api"

sourceUrl :: Url
sourceUrl = "https://www.vpngate.net/api/iphone/"
