module Data.CSTRewrite (rewrite) where

import Data.CSTRewrite.Rule (ImportRule (from, to), ImportRules, ModuleRenameRules, Rules (importRules, moduleRenameRules), fromModuleName, toModuleName)
import Data.Foldable (fold)
import qualified Language.PureScript.CST.Types as PS
import qualified Language.PureScript.Names as N

-- The default monoid for functions relies on a Monoid for the return type
-- and is implemented as
-- @since 2.01
-- instance Monoid b => Monoid (a -> b) where
--   mempty _ = mempty
-- We want a new behavior, where we form a monoid under composition instead,
-- since we know that the functions are endofunctors
newtype Endo a = Endo {unEndo :: a -> a}

instance Semigroup (Endo a) where
  (<>) (Endo f) (Endo g) = Endo (f . g)

instance Monoid (Endo a) where
  mempty = Endo id

mkModuleNameReplacer :: Eq e => ModuleRenameRules e -> Endo (PS.ImportDecl e)
mkModuleNameReplacer rules =
  let froms = fromModuleName <$> rules
      tos = toModuleName <$> rules
      partials = Endo <$> zipWith renameModule froms tos
   in fold partials

renameModule :: Eq e => N.ModuleName -> N.ModuleName -> PS.ImportDecl e -> PS.ImportDecl e
renameModule from to decl =
  let startImp = PS.impModule decl
   in if (PS.nameValue startImp == from) then decl {PS.impModule = startImp {PS.nameValue = to}} else decl

rewrite :: (Eq e, Show e) => Rules e -> PS.Module e -> PS.Module e
rewrite rules mod =
  let moduleRenames = moduleRenameRules rules
      replacer = mkModuleNameReplacer moduleRenames
      replaced = unEndo replacer <$> PS.modImports mod
   in mod {PS.modImports = replaced}
