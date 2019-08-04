module Types where

import Data.Text (Text)
import Data.String

data Error = forall a. Show a => Error { errorRaw :: a }

deriving instance Show Error

newtype Domain = Domain { domainText :: Text } deriving (Show, Eq, Ord)
