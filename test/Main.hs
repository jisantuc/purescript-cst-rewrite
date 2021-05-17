{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.CSTRewrite (rewrite)
import Data.CSTRewrite.Parser (parseModuleRename)
import Data.CSTRewrite.Rule (ModuleRenameRule (toModuleName), Rules (Rules), fromModuleName)
import Data.Functor (void)
import qualified Data.Text.IO as T
import GHC.IO.Exception (ExitCode (..))
import qualified Language.PureScript.CST.Lexer as PS
import qualified Language.PureScript.CST.Parser as PS
import qualified Language.PureScript.CST.Types as PS
import Language.PureScript.Names as N
import System.Exit (exitWith)
import Test.Hspec (describe, hspec, it, shouldBe, shouldContain, shouldNotContain)
import Text.Parsec (parse)
import Text.Parsec.Error (errorMessages, messageString)

main :: IO ()
main = hspec $ do
  describe "rename modules" $ do
    it "parses a sample module rename rule" $ do
      testRuleParser
    it "replaces the old module name in imports with the new one" $ do
      testImportRewrite

testImportRewrite :: IO ()
testImportRewrite = do
  inModuleText <- T.readFile "./test/data/example.purs"
  let lexed = PS.lex inModuleText
  let parsedModule = PS.parseModule lexed
  inRuleText <- T.readFile "./test/data/module-rename-single.diff"
  let parsedRule = parse parseModuleRename "./test/data/module-rename-single.diff" inRuleText
  rule <-
    either
      (\_ -> fail "can't parse rule" *> exitWith (ExitFailure 1))
      ( \rule ->
          pure rule
      )
      parsedRule
  void $
    either
      (\_ -> fail "couldn't parse example PureScript module -- ensure it is valid PureScript")
      ( \m ->
          let partial = PS.resPartial m
              rewritten = rewrite (Rules [] [rule]) partial
              moduleNames = (PS.nameValue . PS.impModule) <$> PS.modImports rewritten
           in do
                _ <- moduleNames `shouldContain` ([toModuleName rule])
                moduleNames `shouldNotContain` ([fromModuleName rule])
      )
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
