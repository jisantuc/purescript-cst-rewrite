module Data.CSTRewrite (rewrite, rewriteFile) where

import Data.CSTRewrite.Parser (readModuleFromPath)
import Data.CSTRewrite.Rule (ModuleRenameRules, Rules, fromModuleName, moduleRenameRules, toModuleName)
import Data.Monoid (Endo (..), appEndo)
import qualified Data.Text.IO as T
import qualified Language.PureScript.CST.Print as PS
import qualified Language.PureScript.CST.Types as PS
import qualified Language.PureScript.Names as N

mkModuleNameReplacer :: (Eq e, Show e) => ModuleRenameRules e -> Endo (PS.ImportDecl e)
mkModuleNameReplacer rules =
  let froms = fromModuleName <$> rules
      tos = toModuleName <$> rules
   in foldMap Endo $ zipWith renameModule froms tos

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

rewrite :: (Eq e, Show e) => Rules e -> PS.Module e -> PS.Module e
rewrite rules psModule =
  let moduleRenames = moduleRenameRules rules
      replacer = mkModuleNameReplacer moduleRenames
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
