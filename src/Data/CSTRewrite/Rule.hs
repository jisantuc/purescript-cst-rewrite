module Data.CSTRewrite.Rule where

import qualified Language.PureScript.CST.Types as PS
import qualified Language.PureScript.Names as N

data ModuleRenameRule e = ModuleRenameRule {fromModuleName :: N.ModuleName, toModuleName :: N.ModuleName} deriving (Eq, Show)

type ModuleRenameRules e =
  [ModuleRenameRule e]

data ImportRenameRule e = ImportRenameRule {fromImportName :: PS.Ident, toImportName :: PS.Ident} deriving (Eq, Show)

type ImportRenameRules e = [ImportRenameRule e]

data Rules e = Rules
  { moduleRenameRules :: ModuleRenameRules e,
    importRenameRules :: ImportRenameRules e
  }
  deriving (Eq, Show)
