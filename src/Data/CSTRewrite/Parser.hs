{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.CSTRewrite.Parser (readRulesFromPath, readModuleFromPath) where

import Data.CSTRewrite.Rule (Rule (..), Rules)
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

-- Getting identifiers out of imports requires going into some pretty
-- nested structure, so it lives ina  helper function here
getImportIdents :: PS.ImportDecl () -> [PS.Ident]
getImportIdents decl =
  let unSeparate sep = PS.sepHead sep : (snd <$> PS.sepTail sep)
      names = snd <$> PS.impNames decl
      unwrapped = unSeparate . PS.wrpValue <$> names
   in ( \case
          Nothing -> []
          Just imports ->
            imports
              >>= ( \case
                      PS.ImportValue _ name -> [PS.nameValue name]
                      _ -> []
                  )
      )
        unwrapped

parserState :: [PS.LexResult] -> PS.ParserState
parserState lexed = PS.ParserState lexed [] []

fromLine :: RuleParser String
fromLine = string "--- from" <* endOfLine

toLine :: RuleParser String
toLine = string "+++ to" <* endOfLine

oldLineAsString :: RuleParser String
oldLineAsString = char '-' *> manyTill anyChar endOfLine

newLineAsString :: RuleParser String
newLineAsString = char '+' *> manyTill anyChar (() <$ try endOfLine <|> eof)

parseRewrite :: String -> (PS.ImportDecl () -> PS.ImportDecl () -> RuleParser (Rules ())) -> RuleParser (Rules ())
parseRewrite s f = do
  _ <- string ("# " <> s) <* endOfLine
  _ <- fromLine
  oldImportLine <- oldLineAsString
  _ <- toLine
  newImportLine <- newLineAsString
  let (oldState, oldImportDeclResult) = PS.runParser (parserState $ PS.lex (pack oldImportLine)) PS.parseImportDeclP
  checkAcceptableSurplus oldState
  let (newState, newImportDeclResult) = PS.runParser (parserState $ PS.lex (pack newImportLine)) PS.parseImportDeclP
  checkAcceptableSurplus newState
  case (oldImportDeclResult, newImportDeclResult) of
    (Right old, Right new) ->
      f old new
    (Left old, Left new) ->
      parserFail $ show old ++ show new
    (Left old, _) ->
      parserFail $ show old
    (_, Left new) ->
      parserFail $ show new

parseModuleRename :: RuleParser (Rules ())
parseModuleRename =
  parseRewrite
    "module rename"
    ( \old new ->
        pure . pure $
          ModuleRenameRule
            (PS.nameValue . PS.impModule $ old)
            (PS.nameValue . PS.impModule $ new)
    )

parseImportRename :: RuleParser (Rules ())
parseImportRename =
  parseRewrite
    "import rename"
    ( \old new ->
        let oldNames = getImportIdents old
            newNames = getImportIdents new
         in pure $ zipWith ImportRenameRule oldNames newNames
    )

parseRules :: RuleParser (Rules ())
parseRules = mconcat <$> sepBy1 (try parseModuleRename <|> parseImportRename) endOfLine

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
