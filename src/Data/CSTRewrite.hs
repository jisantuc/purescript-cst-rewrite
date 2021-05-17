module Data.CSTRewrite (rewrite) where

import Data.CSTRewrite.Rule (ImportRule (from, to), ImportRules, Rules (importRules))
import Data.Foldable (fold)
import Debug.Trace (trace)
import qualified Language.PureScript.CST.Types as PS

replace :: Eq a => a -> a -> [a] -> [a]
replace from to [] = []
replace from to (x : xs) =
  if x == from then to : replace from to xs else x : replace from to xs

mkReplacer :: Eq e => ImportRules e -> [PS.ImportDecl e] -> [PS.ImportDecl e]
mkReplacer rules =
  let froms = from <$> rules
      tos = to <$> rules
      partials = zipWith replace froms tos
   in fold partials

rewrite :: (Eq e, Show e) => Rules e -> PS.Module e -> PS.Module e
rewrite rules mod =
  let irs = importRules rules
      replacer = mkReplacer irs
      replaced = replacer $ PS.modImports mod
   in trace ("Replaced: " ++ show replaced) $
        trace ("Rules: " ++ show irs) $
          mod {PS.modImports = replacer $ PS.modImports mod}
