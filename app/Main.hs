{-# LANGUAGE OverloadedStrings #-}
module Main where

import App
import Options.Applicative
import Data.Semigroup ((<>))
import Option

clientPortOpt :: Parser Int
clientPortOpt = option auto (long "clientPort" <> help "Int")

clientHostOpt :: Parser String
clientHostOpt = option auto (long "clientHost" <> help "Int")

gbOpt :: Parser Int
gbOpt = option auto (long "serverPort" <> help "Int")

sample :: Parser Option
sample = Option <$> gbOpt <*> clientHostOpt <*> clientPortOpt

opts :: ParserInfo Option
opts = info (sample <**> helper) ( fullDesc
  <> progDesc "Print a greeting for TARGET"
      <> header "hello - a test for optparse-applicative" )

main :: IO ()
main = execParser opts >>= App.run

