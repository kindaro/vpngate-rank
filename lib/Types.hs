module Types where

import RIO

import Data.Text (Text)

data Error = forall a. Show a => Error { errorRaw :: a }

deriving instance Show Error
instance Exception Error

newtype Domain = Domain { domainText :: Text } deriving (Show, Eq, Ord)

data Meta

class HasTmpDir a

data EncodingException = EncodingException String deriving Show

instance Exception EncodingException where
    displayException (EncodingException s) = s
