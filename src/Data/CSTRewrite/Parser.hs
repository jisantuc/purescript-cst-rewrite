{-# LANGUAGE OverloadedStrings #-}

module Data.CSTRewrite.Parser where

import Data.CSTRewrite.Rule (ModuleRenameRule (ModuleRenameRule), Rules (Rules))
import Data.Functor.Identity (Identity)
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

-- what's the game? re-use the existing parser for ImportDecls
-- to parse a git diff-style document showing the old import and
-- the new import.
-- the idea here is that users will get to specify the old and
-- new imports in the familiar purescript style, and I won't have
-- to invent a format for the planned change.

parserState :: [PS.LexResult] -> PS.ParserState
parserState lexed = PS.ParserState lexed [] []

parseModuleRename :: RuleParser (ModuleRenameRule ())
parseModuleRename = do
  _ <- string "# module rename" <* endOfLine
  _ <- string "--- from" <* endOfLine
  oldImportLine <- char '-' *> manyTill anyChar endOfLine
  _ <- string "+++ to" <* endOfLine
  newImportLine <- char '+' *> manyTill anyChar (() <$ try endOfLine <|> eof)
  let (_, oldImportDeclResult) = PS.runParser (parserState $ PS.lex (pack oldImportLine)) PS.parseImportDeclP
  let (_, newImportDeclResult) = PS.runParser (parserState $ PS.lex (pack newImportLine)) PS.parseImportDeclP
  case (oldImportDeclResult, newImportDeclResult) of
    (Right old, Right new) ->
      pure $ ModuleRenameRule (PS.nameValue . PS.impModule $ old) (PS.nameValue . PS.impModule $ new)
    (Left old, Left new) ->
      parserFail $ show old ++ show new
    (Left old, _) ->
      parserFail $ show old
    (_, Left new) ->
      parserFail $ show new

parseRules :: RuleParser (Rules ())
parseRules = do
  Rules <$> sepBy1 parseModuleRename endOfLine

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
