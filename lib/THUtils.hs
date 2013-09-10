module THUtils where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

quoteThisFile :: Q Exp
quoteThisFile = do
  loc <- location
  str <- runIO (readFile (loc_filename loc))
  liftString str
  
