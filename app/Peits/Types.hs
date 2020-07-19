{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Peits.Types where

import Data.Aeson ((.!=), (.:), (.:?), (.=))
import qualified Data.Aeson as Ae
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.String.Interpolate.IsString (i)
import Data.Text (Text)
import qualified Data.Text as Tx
import qualified Data.Time.Clock as T
import qualified Data.Time.Format as TF
import qualified Data.Time.Format.ISO8601 as TF.Iso
import qualified Data.Vector as V
import Development.Shake.FilePath ((<.>), (</>))
import qualified Text.Mustache as Mu

parseDate :: DateFmt -> Text -> Maybe T.UTCTime
parseDate (DateFmt fmt) datestr =
  TF.parseTimeM True TF.defaultTimeLocale (Tx.unpack fmt) (Tx.unpack datestr)

showDate :: DateFmt -> T.UTCTime -> Text
showDate (DateFmt fmt) date =
  Tx.pack $ TF.formatTime TF.defaultTimeLocale (Tx.unpack fmt) date

showDateIso :: T.UTCTime -> Text
showDateIso = Tx.pack . TF.Iso.iso8601Show

postDateReadFmt :: DateFmt
postDateReadFmt = DateFmt "%0Y-%m-%d"

-- | January 2, 2020
postDateShowLongFmt :: DateFmt
postDateShowLongFmt = DateFmt "%B %e,%0Y"

-- | 2020.01.02
postDateShowShortFmt :: DateFmt
postDateShowShortFmt = DateFmt "%0Y.%m.%d"

newtype Md = Md {unMd :: Text}

newtype Html = Html {unHtml :: Text}
  deriving (Show)

newtype DateFmt = DateFmt {unDateFmt :: Text}

data Meta = Meta
  { mId :: Text,
    mTitle :: Text,
    mDate :: T.UTCTime,
    mTags :: Set.Set Tag,
    mDesc :: Maybe Text
  }
  deriving (Show)

instance Ae.FromJSON Meta where
  parseJSON = Ae.withObject "Meta" $ \v -> do
    mId <- v .: "id"
    mTitle <- v .: "title"
    dateStr <- v .: "date"
    mDate <- case parseDate postDateReadFmt dateStr of
      Just d -> pure d
      Nothing ->
        fail [i|Cannot parse date "#{dateStr}" for post titled "#{mTitle}"|]
    mTags <- v .:? "tags" .!= mempty
    mDesc <- v .:? "description"
    pure Meta {..}

instance Ae.ToJSON Meta where
  toJSON Meta {..} =
    Ae.object
      [ "id" .= mId,
        "title" .= mTitle,
        "date_long" .= showDate postDateShowLongFmt mDate,
        "date_short" .= showDate postDateShowShortFmt mDate,
        "date_iso" .= showDateIso mDate,
        "tags" .= Set.map TagLink mTags,
        "description" .= mDesc
      ]

newtype Tag = Tag {unTag :: Text}
  deriving stock (Eq, Ord, Show)
  deriving newtype (Ae.FromJSON, Ae.ToJSON)

newtype TagLink = TagLink {unTagLink :: Tag}
  deriving stock (Eq, Ord)

instance Ae.ToJSON TagLink where
  toJSON (TagLink (Tag tag)) =
    Ae.object
      [ "name" .= tag,
        "html_file" .= Tx.pack htmlFile
      ]
    where
      htmlFile = "/tags" </> Tx.unpack tag <.> "html"

newtype TagList = TagList {unTagList :: [TagLink]}

mkTagList :: Set.Set Tag -> TagList
mkTagList =
  TagList
    . sortOn (unTag . unTagLink)
    . fmap TagLink
    . Set.toList

instance Ae.ToJSON TagList where
  toJSON (TagList tags) = Ae.object ["tags" .= tagListJSON]
    where
      tagListJSON =
        Ae.Array . V.fromList . fmap Ae.toJSON $ tags

data Post = Post
  { postMeta :: Meta,
    postMdFile :: FilePath,
    postHtmlFile :: FilePath,
    postRendered :: Html
  }

newtype PostList = PostList {unPostList :: [Post]}

instance Ae.ToJSON PostList where
  toJSON (PostList posts) = Ae.object ["posts" .= postListJSON]
    where
      postListJSON =
        Ae.Array . V.fromList . fmap postJSON $ posts
      postJSON :: Post -> Ae.Value
      postJSON Post {..} =
        Ae.object
          [ "title" .= mTitle postMeta,
            "date_long" .= showDate postDateShowLongFmt (mDate postMeta),
            "date_short" .= showDate postDateShowShortFmt (mDate postMeta),
            "tags" .= unTagList (mkTagList $ mTags postMeta),
            "description" .= mDesc postMeta,
            "md_file" .= postMdFile,
            "html_file" .= postHtmlFile
          ]

data Page = Page
  { pageMeta :: PageMeta,
    pageMdFile :: FilePath,
    pageHtmlFile :: FilePath,
    pageRendered :: Html
  }

data PageMeta = PageMeta
  { pgmId :: Text,
    pgmTitle :: Text
  }

instance Ae.FromJSON PageMeta where
  parseJSON = Ae.withObject "PageMeta" $ \v -> do
    PageMeta
      <$> v .: "id"
      <*> v .: "title"

instance Ae.ToJSON PageMeta where
  toJSON PageMeta {..} =
    Ae.object
      [ "id" .= pgmId,
        "title" .= pgmTitle
      ]

data Config = Config
  { cSiteTitle :: Text,
    cAuthor :: Text,
    cEmail :: Text,
    cCopyright :: Text,
    cHost :: Text,
    cNav :: [NavItem],
    cLists :: Map.Map Text ListPage
  }

instance Ae.FromJSON Config where
  parseJSON = Ae.withObject "Config" $ \v ->
    Config
      <$> v .: "site_title"
      <*> v .: "author"
      <*> v .: "email"
      <*> v .: "copyright"
      <*> v .: "host"
      <*> v .: "nav"
      <*> v .: "lists"

instance Ae.ToJSON Config where
  toJSON Config {..} =
    Ae.object
      [ "site_title" .= cSiteTitle,
        "author" .= cAuthor,
        "email" .= cEmail,
        "copyright" .= cCopyright,
        "host" .= cHost,
        "nav" .= cNav,
        "lists" .= cLists
      ]

data NavItem = NavItem
  { niName :: Text,
    niUrl :: Text
  }

instance Ae.FromJSON NavItem where
  parseJSON = Ae.withObject "NavItem" $ \v ->
    NavItem
      <$> v .: "name"
      <*> v .: "url"

instance Ae.ToJSON NavItem where
  toJSON NavItem {..} =
    Ae.object
      [ "name" .= niName,
        "url" .= niUrl
      ]

data ListPage = ListPage
  { lTitle :: Text,
    lTemplate :: Mu.PName,
    lData :: Ae.Value
  }

instance Ae.FromJSON ListPage where
  parseJSON = Ae.withObject "ListPage" $ \v ->
    ListPage
      <$> v .: "title"
      <*> fmap Mu.PName (v .: "template")
      <*> v .: "data"

instance Ae.ToJSON ListPage where
  toJSON ListPage {..} =
    Ae.object
      [ "title" .= lTitle,
        "data" .= lData
      ]

data Feed = Feed
  { fFile :: Text,
    fUpdated :: T.UTCTime
  }

instance Ae.ToJSON Feed where
  toJSON Feed {..} =
    Ae.object
      [ "feed_file" .= fFile,
        "feed_updated" .= showDateIso fUpdated
      ]
