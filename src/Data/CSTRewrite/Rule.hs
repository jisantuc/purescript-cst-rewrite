module Data.CSTRewrite.Rule where

import qualified Language.PureScript.CST.Types as PS
import qualified Language.PureScript.Names as N

data ModuleRenameRule e = ModuleRenameRule {fromModuleName :: N.ModuleName, toModuleName :: N.ModuleName} deriving (Eq, Show)

type ModuleRenameRules e =
  [ModuleRenameRule e]

-- working with:
-- ImportValue () (
--   Name {
--     nameTok = SourceToken {tokAnn = TokenAnn {tokRange = SourceRange {srcStart = SourcePos {srcLine = 1, srcColumn = 17}, srcEnd = SourcePos {srcLine = 1, srcColumn = 20}}, tokLeadingComments = [], tokTrailingComments = []}, tokValue = TokLowerName [] "baz"}, nameValue = Ident {getIdent = "baz"}
--   }
-- )
data ImportRenameRule e = ImportRenameRule {fromImportName :: PS.Ident, toImportName :: PS.Ident} deriving (Eq, Show)

type ImportRenameRules e = [ImportRenameRule e]

data Rules e = Rules
  { moduleRenameRules :: ModuleRenameRules e,
    importRenameRules :: ImportRenameRules e
  }
  deriving (Eq, Show)
