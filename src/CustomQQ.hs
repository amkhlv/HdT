module CustomQQ where

import Language.Haskell.TH.Quote
import PyF
import PyF.Internal.QQ

cssFmt :: QuasiQuoter
cssFmt =
  mkFormatter
    "fmtWithDelimiters"
    ( fmtConfig
        { delimiters = Just ('$', '$')
        }
    )
