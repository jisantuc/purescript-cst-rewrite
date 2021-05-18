{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.CSTRewrite (rewriteFile)
import Data.CSTRewrite.Parser (readRulesFromPath)
import Options.Applicative
  ( Parser,
    execParser,
    fullDesc,
    header,
    helper,
    info,
    metavar,
    progDesc,
    some,
    strArgument,
    (<**>),
  )

data Options = Options
  { rulePath :: FilePath,
    srcPaths :: [FilePath]
  }

options :: Parser Options
options =
  Options
    <$> strArgument (metavar "RULEPATH")
    <*> ( some $
            strArgument (metavar "SRCPATHS")
        )

run :: Options -> IO ()
run opts = do
  rules <- readRulesFromPath (rulePath opts)
  (rewriteFile rules) `traverse` srcPaths opts
    >>= ( \results ->
            print $ "Rewrote " <> show (length results) <> " files"
        )

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
