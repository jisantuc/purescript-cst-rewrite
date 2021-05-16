module Data.CSTRewrite.Rule where

import qualified Language.PureScript.CST.Types as PS

data ImportRule e
  = RewriteImport { from :: PS.ImportDecl e, to :: PS.ImportDecl e}

type ImportRules e
  = [ImportRule e]

data Rules e
  = Rules { importRules :: ImportRules e }
