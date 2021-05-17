{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.CSTRewrite (rewrite)
import Data.CSTRewrite.Parser (parseModuleRename)
import Data.CSTRewrite.Rule (ModuleRenameRule (toModuleName), Rules (Rules), fromModuleName)
import qualified Data.Text.IO as T
import GHC.IO.Exception (ExitCode (..))
import qualified Language.PureScript.CST.Lexer as PS
import qualified Language.PureScript.CST.Parser as PS
import qualified Language.PureScript.CST.Print as PS
import Language.PureScript.Names as N
import System.Exit (exitWith)
import Test.Hspec (describe, hspec, it, shouldBe)
import Text.Parsec (parse)
import Text.Parsec.Error (errorMessages, messageString)

main :: IO ()
main = hspec $ do
  describe "parse a rule" $ do
    it "parses a sample rule" $ do
      testRuleParser

-- ruleParseTest <- testRuleParser
-- rewriteTest <- testImportRewrite

-- checkRewrite :: (PS.Module -> a) -> PS.Module -> PS.Module -> a ->

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

testRuleParser :: IO ()
testRuleParser = do
  inRuleText <- T.readFile "./test/data/module-rename-single.diff"
  let parsedRule = parse parseModuleRename "./test/data/module-rename-single.diff" inRuleText
  either
    ( \err ->
        let messages = errorMessages err
         in fail $ "Could not parse sample rule: " <> foldMap messageString messages
    )
    ( \rule -> do
        fromModuleName rule `shouldBe` (N.ModuleName "Foo")
        toModuleName rule `shouldBe` (N.ModuleName "Foo.Lib")
    )
    parsedRule
