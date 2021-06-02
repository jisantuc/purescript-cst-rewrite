{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (mzero)
import Data.CSTRewrite (rewrite)
import Data.CSTRewrite.Parser (readModuleFromPath, readRulesFromPath)
import Data.CSTRewrite.Rule
  ( Rule (ImportRenameRule, ModuleRenameRule, toImportName),
    fromImportName,
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
    xit,
  )

main :: IO ()
main = hspec $ do
  describe "rename modules" $ do
    it "parses a sample module rename rule" testModuleRenameRuleParser
    it "replaces the old module name in imports with the new one" testModuleNameRewrite
  describe "rename imports" $ do
    it "parses a sample import rename rule" testImportRenameRuleParser
    it "parses an import rule for plural imports" testImportRenameRulePluralParser
    it "renames a single import" testSingleImportRename
    xit "renames plural imports" (mzero :: IO ())
  describe "apply staged renames" $ do
    it "parses a rule file with more than one rule defined" testMixedRuleParse
    xit "applies renames successively" (mzero :: IO ())
    xit "gets back to the beginning with a circular rename rule" (mzero :: IO ())

testModule :: FilePath -> FilePath
testModule = ("./test/data/example-modules/" <>)

getModuleName :: PS.ImportDecl a -> ModuleName
getModuleName = PS.nameValue . PS.impModule

getImportNames :: PS.Import a -> [PS.Ident]
getImportNames =
  ( \case
      PS.ImportValue _ (PS.Name _ ident) -> [ident]
      _ -> []
  )

getImportDeclNames :: PS.ImportDecl a -> [PS.Ident]
getImportDeclNames decl =
  let maybeNames = PS.impNames decl
   in case maybeNames of
        Just names ->
          ( \case
              (_, PS.Wrapped _ (PS.Separated (PS.ImportValue _ (PS.Name _ v)) t) _) ->
                v : (foldMap getImportNames . fmap snd $ t)
              _ -> []
          )
            names
        Nothing -> []

testModuleNameRewrite :: IO ()
testModuleNameRewrite = do
  psModule <- readModuleFromPath $ testModule "foo-test.purs"
  rules <- readRulesFromPath "./test/data/module-rename-single.diff"

  let rewritten = rewrite rules psModule
      moduleNames = getModuleName <$> PS.modImports rewritten
      rule = head rules
   in do
        moduleNames `shouldContain` [toModuleName rule]
        moduleNames `shouldNotContain` [fromModuleName rule]
        withSystemTempDirectory "rewrite-test" $ \dirPath ->
          do
            fp <- writeTempFile dirPath "rewrite-test" (T.unpack $ PS.printModule rewritten)
            rewrittenFromFile <- readModuleFromPath fp
            let rewrittenModNames = getModuleName <$> PS.modImports rewrittenFromFile
            rewrittenModNames `shouldContain` [toModuleName rule]
            rewrittenModNames `shouldNotContain` [fromModuleName rule]

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

testImportRenameRulePluralParser :: IO ()
testImportRenameRulePluralParser = do
  rules <- readRulesFromPath "./test/data/import-rename-plural.diff"
  let checks = zip [(PS.Ident "bar", PS.Ident "qux"), (PS.Ident "baz", PS.Ident "quux")] rules
  foldMap
    ( \case
        ((from, to), ImportRenameRule f t) -> do
          f `shouldBe` from
          t `shouldBe` to
        (_, r) ->
          unexpectedRule r
    )
    checks

testMixedRuleParse :: IO ()
testMixedRuleParse = do
  rules <- readRulesFromPath "./test/data/staged-rename.diff"
  case rules of
    [ModuleRenameRule modFrom modTo, ImportRenameRule impFrom impTo] ->
      do
        modFrom `shouldBe` N.ModuleName "Foo"
        modTo `shouldBe` N.ModuleName "Foo.Lib"
        impFrom `shouldBe` PS.Ident "bar"
        impTo `shouldBe` PS.Ident "baz"
    rs ->
      fail $
        "Expected exactly a module rename rule, than an import rename rule. Got: " <> show rs

testSingleImportRename :: IO ()
testSingleImportRename = do
  psModule <- readModuleFromPath $ testModule "foo-simple-import-rename.purs"
  rules <- readRulesFromPath "./test/data/import-rename-single.diff"

  let rewritten = rewrite rules psModule
      imports = PS.modImports rewritten
      rule = head rules
      idents = getImportDeclNames =<< imports
   in do
        idents `shouldContain` [toImportName rule]
        idents `shouldNotContain` [fromImportName rule]
        withSystemTempDirectory "rewrite-test" $ \dirPath ->
          do
            fp <- writeTempFile dirPath "rewrite-test" (T.unpack $ PS.printModule rewritten)
            rewrittenFromFile <- readModuleFromPath fp
            let rewrittenImportNames = getImportDeclNames =<< PS.modImports rewrittenFromFile
            rewrittenImportNames `shouldContain` [toImportName rule]
            rewrittenImportNames `shouldNotContain` [fromImportName rule]

unexpectedRule :: MonadFail m => Rule () -> m ()
unexpectedRule rule =
  fail $
    "Encountered an unexpected rule while parsing. Expected a ModuleRenameRule, got: "
      <> show rule
