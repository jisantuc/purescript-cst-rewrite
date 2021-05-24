{-# LANGUAGE OverloadedStrings #-}

module Data.CSTRewrite.Parser where

import Data.CSTRewrite.Rule (ModuleRenameRule (ModuleRenameRule), Rules (Rules))
import Data.Functor.Identity (Identity)
import qualified Data.Set as Set
import Data.Text (Text, pack)
import qualified Data.Text.IO as T
import qualified Language.PureScript.CST.Errors as PS
import qualified Language.PureScript.CST.Lexer as PS
import qualified Language.PureScript.CST.Monad as PS
import qualified Language.PureScript.CST.Parser as PS
import qualified Language.PureScript.CST.Types as PS
import Text.Parsec (ParsecT, anyChar, endOfLine, eof, manyTill, parse, parserFail, sepBy1, try, (<|>))
import Text.Parsec.Error (errorMessages, messageString)
import Text.ParserCombinators.Parsec.Char (char, string)

type RuleParser = ParsecT Text () Identity

-- Rule lines should be wholly consumed except for EOF.
-- If there's extra data in the rule line that's _not_
-- an EOF (e.g., when parsing:
-- import Foo hi how's it going buddy
-- ) that probably indicates user error. As a result, the
-- rule parser should fail in such a circumstance. The resulting
-- error message here isn't quite pretty, but it at least communicates
-- that something unexpected happened.
checkAcceptableSurplus :: PS.ParserState -> RuleParser ()
checkAcceptableSurplus state =
  case sequence $ PS.parserBuff state of
    Right tokens ->
      let acceptableTokens = Set.singleton PS.TokEof
          allExtraTokens = Set.fromList (PS.tokValue <$> tokens) `Set.union` acceptableTokens
       in if (allExtraTokens == acceptableTokens)
            then pure ()
            else
              parserFail $
                "Extra token encountered while parsing a rule: "
                  <> show
                    (allExtraTokens `Set.difference` acceptableTokens)
    Left (_, err) -> parserFail $ PS.prettyPrintError err

parserState :: [PS.LexResult] -> PS.ParserState
parserState lexed = PS.ParserState lexed [] []

parseModuleRename :: RuleParser (ModuleRenameRule ())
parseModuleRename = do
  _ <- string "# module rename" <* endOfLine
  _ <- string "--- from" <* endOfLine
  oldImportLine <- char '-' *> manyTill anyChar endOfLine
  _ <- string "+++ to" <* endOfLine
  newImportLine <- char '+' *> manyTill anyChar (() <$ try endOfLine <|> eof)
  let (oldState, oldImportDeclResult) = PS.runParser (parserState $ PS.lex (pack oldImportLine)) PS.parseImportDeclP
  checkAcceptableSurplus oldState
  let (newState, newImportDeclResult) = PS.runParser (parserState $ PS.lex (pack newImportLine)) PS.parseImportDeclP
  checkAcceptableSurplus newState
  case (oldImportDeclResult, newImportDeclResult) of
    (Right old, Right new) ->
      pure $
        ModuleRenameRule
          (PS.nameValue . PS.impModule $ old)
          (PS.nameValue . PS.impModule $ new)
    (Left old, Left new) ->
      parserFail $ show old ++ show new
    (Left old, _) ->
      parserFail $ show old
    (_, Left new) ->
      parserFail $ show new

parseRules :: RuleParser (Rules ())
parseRules = Rules <$> sepBy1 parseModuleRename endOfLine <*> pure []

readRulesFromPath :: FilePath -> IO (Rules ())
readRulesFromPath src = do
  inRuleText <- T.readFile src
  let parsedRules = parse parseRules src inRuleText
  either
    ( fail
        . (\s -> "Unable to read rules from " <> src <> ". Errors: " <> s)
        . foldMap messageString
        . errorMessages
    )
    (pure . id)
    parsedRules

readModuleFromPath :: FilePath -> IO (PS.Module ())
readModuleFromPath src = do
  inModuleText <- T.readFile src
  let lexed = PS.lex inModuleText
  let parsedModule = PS.parseModule lexed
  either
    ( \err ->
        fail $
          "Unable to parse module from "
            <> src
            <> ". Errors: "
            <> (foldMap PS.prettyPrintError err)
    )
    (\parseResult -> pure $ PS.resPartial parseResult)
    parsedModule
