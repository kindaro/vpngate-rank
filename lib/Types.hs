module Types where

import RIO
import Data.Char

-- * Convenient type synonyms.

type Url = String


-- * Various pieces of data.

data Meta


-- * Exceptions.

data EncodingException = EncodingException String deriving Show

instance Exception EncodingException where
    displayException (EncodingException s) = s

data ProcessException = ProcessException String deriving Show

instance Exception ProcessException where
    displayException (ProcessException s) = if null (filter (not . isSpace) s)
        then "Process exited with error."
        else "Process exited with error. StdErr: \n" ++ s

data OpenVpnException = OpenVpnException String deriving Show

instance Exception OpenVpnException
