module Data.CSTRewrite.Parser where

import Data.CSTRewrite.Rule (ModuleRenameRule (ModuleRenameRule))
import Data.Functor.Identity (Identity)
import Data.Text (Text, pack)
import qualified Language.PureScript.CST.Lexer as PS
import qualified Language.PureScript.CST.Monad as PS
import qualified Language.PureScript.CST.Parser as PS
import qualified Language.PureScript.CST.Types as PS
import Text.Parsec (ParsecT, anyChar, endOfLine, eof, manyTill, parserFail, try, (<|>))
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
