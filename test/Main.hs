module Main where

import qualified Data.Text.IO as T
import qualified Language.PureScript.CST.Lexer as PS
import Language.PureScript.CST.Parser (parseModule)
import Data.CSTRewrite (rewrite)

main :: IO ()
main = print "you should write some tests"

testImportRewrite :: IO ()
testImportRewrite = do
    inText <- T.readFile "./data/example.purs"
    let lexed = PS.lex inText
    let parsed = parseModule lexed
    either (\_ -> print "no good") (\m -> const (print "great") (rewrite rules <$> m)) parsed
    where
        rules = undefined