module Main where

import Data.Text (Text)
import Options.Applicative (Parser, execParser, fullDesc, header, helper, info, metavar, progDesc, strArgument, (<**>))

data Options = Options
  { rulePath :: Text,
    srcPath :: Text
  }

options :: Parser Options
options =
  Options
    <$> strArgument (metavar "RULEPATH")
    <*> strArgument (metavar "SRCPATH")

run :: Options -> IO ()
run = const (print "woohoo")

main :: IO ()
main = execParser opts >>= run
  where
    opts =
      info
        (options <**> helper)
        ( fullDesc
            <> progDesc "Automatically refactor PureScript projects based on rules"
            <> header "purescript-fix"
        )
