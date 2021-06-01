module Data.CSTRewrite (rewrite, rewriteFile) where

import Data.CSTRewrite.Parser (readModuleFromPath)
import Data.CSTRewrite.Rule (Rules, fromImportName, fromModuleName, toImportName, toModuleName)
import Data.Monoid (Endo (..), appEndo)
import qualified Data.Text.IO as T
import Debug.Trace (trace)
import qualified Language.PureScript.CST.Print as PS
import qualified Language.PureScript.CST.Types as PS
import qualified Language.PureScript.Names as N

mkModuleNameReplacer :: (Eq e, Show e) => Rules e -> Endo (PS.ImportDecl e)
mkModuleNameReplacer rules =
  let froms = fromModuleName <$> rules
      tos = toModuleName <$> rules
   in foldMap Endo $ zipWith renameModule froms tos

mkImportNameReplacer :: (Eq e, Show e) => Rules e -> Endo (PS.ImportDecl e)
mkImportNameReplacer rules =
  let froms = fromImportName <$> rules
      tos = toImportName <$> rules
   in foldMap Endo $ zipWith renameImport froms tos

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
replaceImportName fromIdent toIdent imp@(PS.ImportValue e name) =
  if (PS.nameValue name == fromIdent)
    then
      let newName = name {PS.nameValue = toIdent}
       in PS.ImportValue e newName
    else imp
replaceImportName _ _ imp = imp

-- replace impKeyword -> SourceToken { }
renameImport :: (Eq e, Show e) => PS.Ident -> PS.Ident -> PS.ImportDecl e -> PS.ImportDecl e
renameImport fromValue toValue decl =
  let names = PS.impNames decl
      imports =
        ( \(t, PS.Wrapped open imports close) ->
            ( t,
              PS.Wrapped
                open
                (replaceImportName fromValue toValue <$> imports)
                close
            )
        )
          <$> names
   in decl {PS.impNames = imports}

rewrite :: (Eq e, Show e) => Rules e -> PS.Module e -> PS.Module e
rewrite rules psModule =
  let importReplacer = mkImportNameReplacer rules
      -- modReplacer = mkModuleNameReplacer rules
      replaced = appEndo importReplacer <$> PS.modImports psModule
   in psModule {PS.modImports = replaced}

rewriteFile :: Rules () -> FilePath -> IO ()
rewriteFile rules src =
  readModuleFromPath src
    >>= ( \psModule ->
            let rewritten = rewrite rules psModule
                newModuleText = PS.printModule rewritten
             in T.writeFile src newModuleText
        )
