module Types where

import RIO

import Data.Text (Text)

type Url = Text

data Meta

class HasTmpDir a

data EncodingException = EncodingException String deriving Show

instance Exception EncodingException where
    displayException (EncodingException s) = s
