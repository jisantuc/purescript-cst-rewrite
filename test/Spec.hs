{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.CSTRewrite (rewrite)
import Data.CSTRewrite.Parser (readModuleFromPath, readRulesFromPath)
import Data.CSTRewrite.Rule
  ( Rule (ImportRenameRule, ModuleRenameRule),
    fromModuleName,
    toModuleName,
  )
import qualified Data.Text as T
import qualified Language.PureScript.CST.Print as PS
import qualified Language.PureScript.CST.Types as PS
import Language.PureScript.Names as N
import System.IO.Temp (withSystemTempDirectory, writeTempFile)
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
      testModuleRenameRuleParser
    it "parses a sample import rename rule" $ do
      testImportRenameRuleParser
    it "replaces the old module name in imports with the new one" $ do
      testImportRewrite

getModuleName :: PS.ImportDecl a -> ModuleName
getModuleName = PS.nameValue . PS.impModule

testImportRewrite :: IO ()
testImportRewrite = do
  psModule <- readModuleFromPath "./test/data/example.purs"
  rules <- readRulesFromPath "./test/data/module-rename-single.diff"

  let rewritten = rewrite rules psModule
      moduleNames = getModuleName <$> PS.modImports rewritten
      rule = head rules
   in do
        moduleNames `shouldContain` ([toModuleName rule])
        moduleNames `shouldNotContain` ([fromModuleName rule])
        withSystemTempDirectory "rewrite-test" $ \dirPath ->
          do
            fp <- writeTempFile dirPath "rewrite-test" (T.unpack $ PS.printModule rewritten)
            rewrittenFromFile <- readModuleFromPath fp
            let rewrittenModNames = getModuleName <$> PS.modImports rewrittenFromFile
            rewrittenModNames `shouldContain` ([toModuleName rule])
            rewrittenModNames `shouldNotContain` ([fromModuleName rule])

testModuleRenameRuleParser :: IO ()
testModuleRenameRuleParser = do
  rules <- readRulesFromPath "./test/data/module-rename-single.diff"
  foldMap
    ( \case
        ModuleRenameRule from to ->
          do
            from `shouldBe` (N.ModuleName "Foo")
            to `shouldBe` (N.ModuleName "Foo.Lib")
        r ->
          unexpectedRule r
    )
    rules

testImportRenameRuleParser :: IO ()
testImportRenameRuleParser = do
  rules <- readRulesFromPath "./test/data/import-rename-single.diff"
  foldMap
    ( \case
        ImportRenameRule from to ->
          do
            from `shouldBe` PS.Ident "bar"
            to `shouldBe` PS.Ident "baz"
        r ->
          unexpectedRule r
    )
    rules

unexpectedRule :: MonadFail m => Rule () -> m ()
unexpectedRule rule =
  fail $
    "Encountered an unexpected rule while parsing. Expected a ModuleRenameRule, got: " <> show rule
