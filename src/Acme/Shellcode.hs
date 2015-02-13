{-# LANGUAGE ForeignFunctionInterface, MonadComprehensions, QuasiQuotes, TemplateHaskell #-}

module Acme.Shellcode (shell, shellQQ) where

import Data.Char
import Foreign.C
import Foreign.Ptr
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Text.Parsec
import Text.Parsec.Language
import Text.Parsec.String
import Text.Parsec.Token

foreign import ccall "shellcode.h shellcode" c_shellcode :: Ptr () -> CSize -> IO ()

parser :: Parser String
parser = many [ chr $ fromIntegral n | n <- integer haskell, 0 <= n && n <= 255 ]

shell :: String -> Q Exp
shell s = [e|withCAStringLen $(stringE s) $ \(buf, len) -> c_shellcode (castPtr buf) (fromIntegral len)|]

shellQQ :: QuasiQuoter
shellQQ = QuasiQuoter { quoteExp = either (fail . show) shell . parse parser ""
                      , quotePat = undefined
                      , quoteType = undefined
                      , quoteDec = undefined
                      }
