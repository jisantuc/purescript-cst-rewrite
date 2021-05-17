module Data.CSTRewrite (rewrite) where

import Data.CSTRewrite.Rule (ImportRule (from, to), ImportRules, Rules (importRules))
import Data.Foldable (fold)
import Debug.Trace (trace)
import qualified Language.PureScript.CST.Types as PS

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

replace :: Eq a => a -> a -> [a] -> [a]
replace from to [] = []
replace from to (x : xs) =
  if x == from then to : replace from to xs else x : replace from to xs

mkReplacer :: Eq e => ImportRules e -> Endo [PS.ImportDecl e]
mkReplacer rules =
  let froms = from <$> rules
      tos = to <$> rules
      partials = Endo <$> zipWith replace froms tos
   in fold partials

rewrite :: (Eq e, Show e) => Rules e -> PS.Module e -> PS.Module e
rewrite rules mod =
  let irs = importRules rules
      replacer = mkReplacer irs
      replaced = unEndo replacer $ PS.modImports mod
   in trace ("Replaced: " ++ show replaced) $
        trace ("Rules: " ++ show irs) $
          mod {PS.modImports = unEndo replacer $ PS.modImports mod}
