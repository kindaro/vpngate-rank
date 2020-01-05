module Types where

import RIO
import Data.Char (isSpace)

type Url = String

data EncodingException = EncodingException String deriving Show

instance Exception EncodingException where
    displayException (EncodingException s) = s

data ProcessException = ProcessException String deriving Show

instance Exception ProcessException where
    displayException (ProcessException s) = if null (filter (not . isSpace) s)
        then "Process exited with error."
        else "Process exited with error. StdErr: \n" ++ s

data TimeoutException = TimeoutException deriving Show

instance Exception TimeoutException where
    displayException _ = "IO action timed out."

data OpenVpnException = OpenVpnException String deriving Show

instance Exception OpenVpnException
