module Data.CSTRewrite.Rule where

import Language.PureScript.CST.Types ( ImportDecl )

data ImportRule e
  = RewriteImport { from :: ImportDecl e, to :: ImportDecl e}

type ImportRules e
  = [ImportRule e]

data Rules e
  = Rules { importRules :: ImportRules e }