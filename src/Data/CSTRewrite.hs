module Data.CSTRewrite (rewrite, rewriteFile) where

import Data.CSTRewrite.Parser (readModuleFromPath)
import Data.CSTRewrite.Rule (Rule (ImportRenameRule, ModuleRenameRule), Rules)
import Data.Monoid (Endo (..), appEndo)
import qualified Data.Text.IO as T
import qualified Language.PureScript.CST.Print as PS
import qualified Language.PureScript.CST.Types as PS
import qualified Language.PureScript.Names as N

mkReplacer :: (Eq e, Show e) => Rule e -> Endo (PS.ImportDecl e)
mkReplacer (ModuleRenameRule from to) = Endo $ renameModule from to
mkReplacer (ImportRenameRule from to) = Endo $ renameImport from to

renameModule :: (Eq e, Show e) => N.ModuleName -> N.ModuleName -> PS.ImportDecl e -> PS.ImportDecl e
renameModule from to decl =
  let startImp = PS.impModule decl
      startTok = PS.nameTok startImp
      toText = N.runModuleName to
   in if (PS.nameValue startImp == from)
        then
          decl
            { PS.impModule =
                startImp {PS.nameValue = to, PS.nameTok = startTok {PS.tokValue = PS.TokUpperName [] toText}}
            }
        else decl

replaceImportName :: (Eq e, Show e) => PS.Ident -> PS.Ident -> PS.Import e -> PS.Import e
replaceImportName fromIdent toIdent imp@(PS.ImportValue e (PS.Name nameToken nameValue)) =
  if (nameValue == fromIdent)
    then
      let newToken = nameToken {PS.tokValue = PS.TokLowerName [] (PS.getIdent toIdent)}
          newName = PS.Name newToken toIdent
       in PS.ImportValue e newName
    else imp
replaceImportName _ _ imp = imp

renameImport :: (Eq e, Show e) => PS.Ident -> PS.Ident -> PS.ImportDecl e -> PS.ImportDecl e
renameImport fromValue toValue decl =
  let names = PS.impNames decl
      imports =
        ( \(t, PS.Wrapped open sep close) ->
            ( t,
              PS.Wrapped
                open
                (replaceImportName fromValue toValue <$> sep)
                close
            )
        )
          <$> names
   in decl {PS.impNames = imports}

rewrite :: (Eq e, Show e) => Rules e -> PS.Module e -> PS.Module e
rewrite rules psModule =
  let replacer = foldMap mkReplacer $ reverse rules
      replaced = appEndo replacer <$> PS.modImports psModule
   in psModule {PS.modImports = replaced}

rewriteFile :: Rules () -> FilePath -> IO ()
rewriteFile rules src =
  readModuleFromPath src
    >>= ( \psModule ->
            let rewritten = rewrite rules psModule
                newModuleText = PS.printModule rewritten
             in T.writeFile src newModuleText
        )
