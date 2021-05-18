module Data.CSTRewrite.Rule where

import qualified Language.PureScript.Names as N

data ModuleRenameRule e = ModuleRenameRule {fromModuleName :: N.ModuleName, toModuleName :: N.ModuleName} deriving (Eq, Show)

type ModuleRenameRules e =
  [ModuleRenameRule e]

data Rules e = Rules
  { moduleRenameRules :: ModuleRenameRules e
  }
  deriving (Eq, Show)
