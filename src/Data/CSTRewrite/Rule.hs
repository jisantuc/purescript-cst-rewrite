module Data.CSTRewrite.Rule where

import Language.PureScript.CST.Types (ImportDecl)
import qualified Language.PureScript.Names as N

data ImportRule e = RewriteImport {from :: ImportDecl e, to :: ImportDecl e} deriving (Eq, Show)

data ModuleRenameRule e = ModuleRenameRule {fromModuleName :: N.ModuleName, toModuleName :: N.ModuleName} deriving (Eq, Show)

type ModuleRenameRules e =
  [ModuleRenameRule e]

type ImportRules e =
  [ImportRule e]

data Rules e = Rules
  { importRules :: ImportRules e,
    moduleRenameRules :: ModuleRenameRules e
  }
