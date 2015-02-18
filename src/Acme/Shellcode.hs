{-# LANGUAGE ForeignFunctionInterface, MonadComprehensions, QuasiQuotes, TemplateHaskell #-}

module Acme.Shellcode (exec, shell) where

import Control.Applicative
import Data.ByteString.Char8
import Data.Char
import Foreign.C
import Foreign.Ptr
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Text.Parsec hiding (many)
import Text.Parsec.Language
import Text.Parsec.String
import Text.Parsec.Token

foreign import ccall "shellcode.h shellcode" c_shellcode :: Ptr () -> CSize -> IO ()

parser :: Parser String
parser = many [ chr $ fromIntegral n | n <- integer haskell, 0 <= n && n <= 255 ] <* eof

exec :: ByteString -> IO ()
exec = flip useAsCStringLen $ \(buf, len) -> c_shellcode (castPtr buf) (fromIntegral len)

shell :: QuasiQuoter
shell = QuasiQuoter { quoteExp = either (fail . show) (\s -> [e|exec $ $(stringE s)|]) . parse parser ""
                    , quotePat = undefined
                    , quoteType = undefined
                    , quoteDec = undefined
                    }
