{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Aeson ((.=))
import qualified Data.Aeson as Ae
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as Tx
import qualified Data.Text.Lazy.IO as Tx.L.IO
import qualified Data.Time as T
import qualified Data.Time.Format as TF
import Options.Applicative ((<**>))
import qualified Options.Applicative as OA
import qualified Text.Mustache as Mu

main :: IO ()
main = do
  options <- OA.execParser $ OA.info (optionParser <**> OA.helper) mempty
  now <- T.getCurrentTime
  let config = mkConfig options now
      filePath = "posts/" <> mkFileName config
  renderTemplate config filePath

optionParser :: OA.Parser Options
optionParser =
  Options <$> idOpt <*> titleOpt <*> descOpt <*> OA.many tagsOpt
  where
    idOpt =
      OA.strOption $
        OA.long "id"
          <> OA.short 'i'
          <> OA.metavar "ID"
          <> OA.help "Post id"
    titleOpt =
      OA.strOption $
        OA.long "title"
          <> OA.metavar "TITLE"
          <> OA.help "Post title"
    descOpt =
      OA.optional . OA.strOption $
        OA.long "description"
          <> OA.short 'd'
          <> OA.metavar "DESC"
          <> OA.help "Post description"
    tagsOpt =
      OA.strOption $
        OA.long "tag"
          <> OA.short 't'
          <> OA.metavar "TAG"
          <> OA.help "Tag associated with post"

templateFile :: FilePath
templateFile = "templates/new_post.mustache"

renderTemplate :: Config -> FilePath -> IO ()
renderTemplate cfg fp = do
  template <- Mu.compileMustacheFile templateFile
  let content = Mu.renderMustache template (Ae.toJSON cfg)
  Tx.L.IO.writeFile fp content

dateFmt :: String
dateFmt = "%0Y-%m-%d"

formatDate :: T.UTCTime -> Text
formatDate =
  Tx.pack . TF.formatTime TF.defaultTimeLocale dateFmt

data Options = Options
  { oId :: Text,
    oTitle :: Text,
    oDesc :: Maybe Text,
    oTags :: [Text]
  }

data Config = Config
  { cId :: Text,
    cTitle :: Text,
    cDesc :: Maybe Text,
    cTags :: [Text],
    cDate :: T.UTCTime
  }

instance Ae.ToJSON Config where
  toJSON Config {..} =
    Ae.object
      [ "id" .= cId,
        "title" .= cTitle,
        "description" .= cDesc,
        "has_tags" .= not (null cTags),
        "tags" .= sort cTags,
        "date" .= formatDate cDate
      ]

mkConfig :: Options -> T.UTCTime -> Config
mkConfig Options {..} date =
  Config
    { cId = oId,
      cTitle = oTitle,
      cDesc = oDesc,
      cTags = oTags,
      cDate = date
    }

mkFileName :: Config -> String
mkFileName Config {..} =
  Tx.unpack (formatDate cDate) <> "-" <> Tx.unpack cId <> ".md"
