module Types where

import RIO

import Data.Text (Text)

type Url = ByteString

data Meta

class HasTmpDir a

data EncodingException = EncodingException String deriving Show

instance Exception EncodingException where
    displayException (EncodingException s) = s

data ProcessException = ProcessException String deriving Show

instance Exception ProcessException where
    displayException (ProcessException s) = s
