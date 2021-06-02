{-# LANGUAGE LambdaCase #-}

module Data.CSTRewrite.Rule where

import qualified Language.PureScript.CST.Types as PS
import qualified Language.PureScript.Names as N

data Rule e
  = ModuleRenameRule {fromModuleName :: N.ModuleName, toModuleName :: N.ModuleName}
  | ImportRenameRule {fromImportName :: PS.Ident, toImportName :: PS.Ident}
  deriving (Eq, Show)

type Rules e = [Rule e]

toModuleNames :: Rules e -> [N.ModuleName]
toModuleNames =
  ( ( \case
        ModuleRenameRule _ to -> [to]
        _ -> []
    )
      =<<
  )

fromModuleNames :: Rules e -> [N.ModuleName]
fromModuleNames =
  ( ( \case
        ModuleRenameRule from _ -> [from]
        _ -> []
    )
      =<<
  )

toImportNames :: Rules e -> [PS.Ident]
toImportNames =
  ( ( \case
        ImportRenameRule _ to -> [to]
        _ -> []
    )
      =<<
  )

fromImportNames :: Rules e -> [PS.Ident]
fromImportNames =
  ( ( \case
        ImportRenameRule from _ -> [from]
        _ -> []
    )
      =<<
  )
