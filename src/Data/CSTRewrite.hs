module Data.CSTRewrite where

import Data.CSTRewrite.Rule ( Rules )
import qualified Language.PureScript.CST.Types as PS

rewrite :: Rules e -> PS.Module e -> PS.Module e
rewrite rules mod = undefined