{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.CSTRewrite (rewrite)
import Data.CSTRewrite.Parser (readModuleFromPath, readRulesFromPath)
import Data.CSTRewrite.Rule
  ( Rule
      ( ImportRenameRule,
        ModuleRenameRule,
        toImportName
      ),
    Rules,
    fromImportName,
    fromImportNames,
    fromModuleName,
    fromModuleNames,
    toImportNames,
    toModuleName,
    toModuleNames,
  )
import Data.Functor (void)
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
    it "parses a sample module rename rule" testModuleRenameRuleParser
    it "replaces the old module name in imports with the new one" testModuleNameRewrite
    it "replaces a longer module name with a shorter one" testModuleShorten
  describe "rename imports" $ do
    it "parses a sample import rename rule" testImportRenameRuleParser
    it "parses an import rule for plural imports" testImportRenameRulePluralParser
    it "renames a single import" testSingleImportRename
    it "renames plural imports" testPluralImportRename
  describe "apply staged renames" $ do
    it "parses a rule file with more than one rule defined" testMixedRuleParse
    it "applies renames successively" testStagedRewrite
    it "gets back to the beginning with a simple circular module rename rule" testSimpleCircularModuleRewrite
    it "gets back to the beginning with a simple circular import rename rule" testSimpleCircularImportRewrite
    it "gets back to the beginning with a complex circular rename rule" testComplexCircularRewrite

testModule :: FilePath -> FilePath
testModule = ("./test/data/example-modules/" <>)

testRule :: FilePath -> FilePath
testRule = ("./test/data/" <>)

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

testModuleRenameRuleParser :: IO ()
testModuleRenameRuleParser = do
  rules <- readRulesFromPath $ testRule "module-rename-single.diff"
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
  rules <- readRulesFromPath $ testRule "import-rename-single.diff"
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
  rules <- readRulesFromPath $ testRule "import-rename-plural.diff"
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
  rules <- readRulesFromPath $ testRule "staged-rename.diff"
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

testModuleNameRewrite :: IO ()
testModuleNameRewrite =
  testRewrite
    "foo-test.purs"
    "module-rename-single.diff"
    (getModuleName <$>)
    ( \moduleNames rules ->
        do
          moduleNames `shouldContain` (toModuleName <$> rules)
          moduleNames `shouldNotContain` (fromModuleName <$> rules)
    )

testSingleImportRename :: IO ()
testSingleImportRename =
  testRewrite
    "foo-simple-import-rename.purs"
    "import-rename-single.diff"
    (getImportDeclNames =<<)
    ( \idents rules ->
        do
          idents `shouldContain` (toImportName <$> rules)
          idents `shouldNotContain` (fromImportName <$> rules)
    )

testPluralImportRename :: IO ()
testPluralImportRename =
  testRewrite
    "foo-multi-import.purs"
    "import-rename-plural.diff"
    (getImportDeclNames =<<)
    ( \idents rules -> do
        void $ traverse (\rule -> idents `shouldNotContain` [fromImportName rule]) rules
        void $ traverse (\rule -> idents `shouldContain` [toImportName rule]) rules
    )

testStagedRewrite :: IO ()
testStagedRewrite =
  testRewrite
    "staged-rename.purs"
    "staged-rename.diff"
    (\decls -> (getModuleName <$> decls, getImportDeclNames =<< decls))
    ( \(moduleNames, idents) rules ->
        do
          moduleNames `shouldContain` toModuleNames rules
          moduleNames `shouldNotContain` fromModuleNames rules
          idents `shouldContain` toImportNames rules
          idents `shouldNotContain` fromImportNames rules
    )

testSimpleCircularModuleRewrite :: IO ()
testSimpleCircularModuleRewrite =
  testRewriteRoundTrip "foo-test.purs" "circular-rename-simple-module.diff"

testSimpleCircularImportRewrite :: IO ()
testSimpleCircularImportRewrite = testRewriteRoundTrip "foo-simple-import-rename.purs" "circular-rename-simple-import.diff"

testComplexCircularRewrite :: IO ()
testComplexCircularRewrite = testRewriteRoundTrip "foo-test.purs" "circular-rename.diff"

testModuleShorten :: IO ()
testModuleShorten =
  testRewrite
    "foo-lib.purs"
    "import-shorten.diff"
    (getModuleName <$>)
    ( \moduleNames rules ->
        do
          moduleNames `shouldContain` (toModuleName <$> rules)
          moduleNames `shouldNotContain` (fromModuleName <$> rules)
    )

testRewriteRoundTrip :: FilePath -> FilePath -> IO ()
testRewriteRoundTrip inModule inRule = do
  psModule <- readModuleFromPath $ testModule inModule
  rules <- readRulesFromPath (testRule inRule)

  let rewritten = rewrite rules psModule
      startImports = PS.modImports psModule
      imports = PS.modImports rewritten
   in do
        imports `shouldBe` startImports
        withSystemTempDirectory "rewrite-test" $ \dirPath ->
          do
            fp <- writeTempFile dirPath "rewrite-test" (T.unpack $ PS.printModule rewritten)
            rewrittenFromFile <- readModuleFromPath fp
            let postRewrite = PS.modImports rewrittenFromFile
            postRewrite `shouldBe` startImports

testRewrite ::
  -- | Where the test module lives
  FilePath ->
  -- | Where the rules file lives
  FilePath ->
  -- | how to get the items of interest from import declarations
  ([PS.ImportDecl ()] -> a) ->
  -- | what to expect about extracted values
  (a -> Rules () -> IO ()) ->
  IO ()
testRewrite modPath rulePath extractor expecter = do
  psModule <- readModuleFromPath $ testModule modPath
  rules <- readRulesFromPath (testRule rulePath)

  let rewritten = rewrite rules psModule
      imports = PS.modImports rewritten
      extracted = extractor imports
   in do
        expecter extracted rules
        withSystemTempDirectory "rewrite-test" $ \dirPath ->
          do
            fp <- writeTempFile dirPath "rewrite-test" (T.unpack $ PS.printModule rewritten)
            rewrittenFromFile <- readModuleFromPath fp
            let postRewrite = extractor $ PS.modImports rewrittenFromFile
            expecter postRewrite rules

unexpectedRule :: MonadFail m => Rule () -> m ()
unexpectedRule rule =
  fail $
    "Encountered an unexpected rule while parsing. Expected a ModuleRenameRule, got: "
      <> show rule
