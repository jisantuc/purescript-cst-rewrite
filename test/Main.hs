module Main where

import Data.CSTRewrite (rewrite)
import Data.CSTRewrite.Parser (parseModuleRename)
import Data.CSTRewrite.Rule (Rules (Rules))
import qualified Data.Text.IO as T
import GHC.IO.Exception (ExitCode (..))
import qualified Language.PureScript.CST.Lexer as PS
import qualified Language.PureScript.CST.Parser as PS
import qualified Language.PureScript.CST.Print as PS
import System.Exit (exitWith)
import Text.Parsec (parse)

main :: IO ()
main = do
  ruleParseTest <- testRuleParser
  rewriteTest <- testImportRewrite
  exitWith (maximum [ruleParseTest, rewriteTest])

testImportRewrite :: IO ExitCode
testImportRewrite = do
  inModuleText <- T.readFile "./test/data/example.purs"
  let lexed = PS.lex inModuleText
  let parsedModule = PS.parseModule lexed
  print . show $ PS.resPartial <$> parsedModule
  inRuleText <- T.readFile "./test/data/module-rename-single.diff"
  let parsedRule = parse parseModuleRename "./test/data/module-rename-single.diff" inRuleText
  rules <-
    either
      (\_ -> print "can't parse rule" *> exitWith (ExitFailure 1))
      ( \rule ->
          pure $ Rules [] [rule]
      )
      parsedRule
  either
    (\_ -> print "couldn't parse module" *> (pure $ ExitFailure 1))
    (\m -> ExitSuccess <$ (print . show $ PS.resPartial (rewrite rules <$> m)))
    parsedModule

testRuleParser :: IO ExitCode
testRuleParser = do
  inRuleText <- T.readFile "./test/data/module-rename-single.diff"
  let parsedRule = parse parseModuleRename "./test/data/module-rename-single.diff" inRuleText
  either
    (\err -> ExitFailure 1 <$ (print $ show err))
    (\_ -> pure ExitSuccess)
    parsedRule
