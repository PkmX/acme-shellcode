{-# LANGUAGE ForeignFunctionInterface, MonadComprehensions, QuasiQuotes, TemplateHaskell #-}

module Acme.Shellcode (shell, shellQQ) where

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

parser :: Parser ByteString
parser = pack <$> many [ chr $ fromIntegral n | n <- integer haskell, 0 <= n && n <= 255 ] <* eof

shell :: ByteString -> Q Exp
shell s = [e|useAsCStringLen $(stringE $ unpack s) $ \(buf, len) -> c_shellcode (castPtr buf) (fromIntegral len)|]

shellQQ :: QuasiQuoter
shellQQ = QuasiQuoter { quoteExp = either (fail . show) shell . parse parser ""
                      , quotePat = undefined
                      , quoteType = undefined
                      , quoteDec = undefined
                      }
