{-# LANGUAGE QuasiQuotes #-}

module Peits.Options
  ( Options (..),
    options,
    getOptions,
  )
where

import Data.Function ((&))
import Data.String.Interpolate (i)
import Peits.Config (SyntaxHighlightMethod (..))
import System.Console.GetOpt (ArgDescr (..), OptDescr (..))

data Options = Options
  { optSyntaxHighlightMethod :: Maybe SyntaxHighlightMethod
  }

defaultOptions :: Options
defaultOptions =
  Options
    { optSyntaxHighlightMethod = Nothing
    }

getOptions :: [Options -> Options] -> Options
getOptions = foldl (&) defaultOptions

options :: [OptDescr (Either String (Options -> Options))]
options =
  [ Option
      ""
      ["highlight"]
      (OptArg readSyntaxHighlightMethod "METHOD")
      "Syntax highlighting method (default/pygments/prismjs)"
  ]

readSyntaxHighlightMethod :: Maybe String -> Either String (Options -> Options)
readSyntaxHighlightMethod maybeMethod = case methodOrErr of
  Right method -> Right (\o -> o {optSyntaxHighlightMethod = Just method})
  Left err -> Left err
  where
    methodOrErr = case maybeMethod of
      Nothing -> Right Pygments
      Just "default" -> Right Default
      Just "pygments" -> Right Pygments
      Just "prismjs" -> Right PrismJS
      Just x -> Left [i|Unknown syntax highlighting method #{x}"|]
