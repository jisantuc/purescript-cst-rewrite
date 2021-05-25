module Data.CSTRewrite.Rule where

import qualified Language.PureScript.CST.Types as PS
import qualified Language.PureScript.Names as N

data Rule e
  = ModuleRenameRule {fromModuleName :: N.ModuleName, toModuleName :: N.ModuleName}
  | ImportRenameRule {fromImportName :: PS.Ident, toImportName :: PS.Ident}
  deriving (Eq, Show)

type Rules e = [Rule e]
