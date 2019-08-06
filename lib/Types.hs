module Types where

import RIO

import Data.Text (Text)

data Error = forall a. Show a => Error { errorRaw :: a }

deriving instance Show Error
instance Exception Error

newtype Domain = Domain { domainText :: Text } deriving (Show, Eq, Ord)
