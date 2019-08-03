module Main where

import Prelude hiding (readFile)
import Data.ByteString.Lazy (readFile)
import qualified Data.ByteString.Lazy as Lazy
import System.Environment
import Data.Csv
import GHC.Generics
import Data.Text (Text)
import qualified Data.Text as Strict
import Data.Word
import Data.Char

main :: IO ()
main = do
    target <- fmap (!! 0) getArgs
    body <- fmap (undefined . Lazy.break (/= (fromIntegral . ord $ '\n'))) (readFile target)
    let parsed = decode @Row HasHeader body
    putStrLn "Done!"

data Row = Row
    { hostName
    , ip
    , score
    , ping
    , speed
    , countryLong
    , countryShort
    , numVpnSessions
    , uptime
    , totalUsers
    , totalTraffic
    , logType
    , operator
    , message
    , openVPN_ConfigData_Base64 :: Text
    } deriving (Generic, Show)

instance FromRecord Row
instance ToRecord Row
