{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.CSTRewrite (rewrite)
import Data.CSTRewrite.Parser (readModuleFromPath, readRulesFromPath)
import Data.CSTRewrite.Rule
  ( ModuleRenameRule (toModuleName),
    Rules (Rules, moduleRenameRules),
    fromModuleName,
  )
import qualified Language.PureScript.CST.Print as PS
import qualified Language.PureScript.CST.Types as PS
import Language.PureScript.Names as N
import Test.Hspec
  ( describe,
    hspec,
    it,
    shouldBe,
    shouldContain,
    shouldNotContain,
  )

main :: IO ()
main = hspec $ do
  describe "rename modules" $ do
    it "parses a sample module rename rule" $ do
      testRuleParser
    it "replaces the old module name in imports with the new one" $ do
      testImportRewrite

testImportRewrite :: IO ()
testImportRewrite = do
  psModule <- readModuleFromPath "./test/data/example.purs"
  rules <- readRulesFromPath "./test/data/module-rename-single.diff"

  let rewritten = rewrite rules psModule
      moduleNames = (PS.nameValue . PS.impModule) <$> PS.modImports rewritten
      rule = head $ moduleRenameRules rules
   in do
        print $ PS.printModule rewritten
        moduleNames `shouldContain` ([toModuleName rule])
        moduleNames `shouldNotContain` ([fromModuleName rule])

testRuleParser :: IO ()
testRuleParser = do
  (Rules rules) <- readRulesFromPath "./test/data/module-rename-single.diff"
  foldMap
    ( \rule -> do
        fromModuleName rule `shouldBe` (N.ModuleName "Foo")
        toModuleName rule `shouldBe` (N.ModuleName "Foo.Lib")
    )
    rules
