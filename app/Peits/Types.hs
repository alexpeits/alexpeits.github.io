{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Peits.Types where

import Data.Aeson ((.!=), (.:), (.:?), (.=))
import qualified Data.Aeson as Ae
import qualified Data.Aeson.KeyMap as KM
import Data.List (sortOn)
import Data.Maybe (isJust)
import qualified Data.Set as Set
import Data.String.Interpolate (i)
import Data.Text (Text)
import qualified Data.Text as Tx
import qualified Data.Time.Clock as T
import qualified Data.Time.Format as TF
import qualified Data.Time.Format.ISO8601 as TF.Iso
import qualified Data.Vector as V
import Development.Shake.FilePath ((<.>), (</>))
import qualified Text.Mustache as Mu

data Theme = Light | Dark

parseDate :: DateFmt -> Text -> Maybe T.UTCTime
parseDate (DateFmt fmt) =
  TF.parseTimeM True TF.defaultTimeLocale (Tx.unpack fmt) . Tx.unpack

showDate :: DateFmt -> T.UTCTime -> Text
showDate (DateFmt fmt) =
  Tx.pack . TF.formatTime TF.defaultTimeLocale (Tx.unpack fmt)

showDateIso :: T.UTCTime -> Text
showDateIso = Tx.pack . TF.Iso.iso8601Show

postDateReadFmt :: DateFmt
postDateReadFmt = DateFmt "%0Y-%m-%d"

-- | January 2, 2020
postDateShowLongFmt :: DateFmt
postDateShowLongFmt = DateFmt "%B %e, %0Y"

-- | 2020.01.02
postDateShowShortFmt :: DateFmt
postDateShowShortFmt = DateFmt "%0Y.%m.%d"

defaultReferenceSectionTitle :: Text
defaultReferenceSectionTitle = "References"

defaultTocTitle :: Text
defaultTocTitle = "Table of contents"

defaultTocDepth :: Int
defaultTocDepth = 2

newtype MdFull = MdFull {unMdFull :: Text}

newtype MdContent = MdContent {unMdContent :: Text}

newtype Html = Html {unHtml :: Text}

newtype DateFmt = DateFmt {unDateFmt :: Text}

data Meta = Meta
  { mId :: Text,
    mTitle :: Text,
    mDate :: T.UTCTime,
    mTags :: Set.Set Tag,
    mDesc :: Maybe Text,
    mToc :: Bool,
    mTocTitle :: Text,
    mTocDepth :: Int,
    mBibliography :: Maybe Ae.Value,
    mReferenceSectionTitle :: Text,
    mData :: Maybe Ae.Value
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
    mToc <- v .:? "toc" .!= False
    mTocTitle <- v .:? "toc-title" .!= defaultTocTitle
    mTocDepth <- v .:? "toc-depth" .!= defaultTocDepth
    mBibliography <- v .:? "bibliography"
    mReferenceSectionTitle <-
      v .:? "reference-section-title" .!= defaultReferenceSectionTitle
    mData <- v .:? "data"
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
        "description" .= mDesc,
        "toc" .= mToc,
        "toc-title" .= mTocTitle,
        "toc-depth" .= mTocDepth,
        "bibliography" .= mBibliography,
        "reference-section-title" .= mReferenceSectionTitle,
        "data" .= mData
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
            "date_iso" .= showDateIso (mDate postMeta),
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

newtype PageList = PageList {unPageList :: [Page]}

instance Ae.ToJSON PageList where
  toJSON (PageList pages) = Ae.object ["pages" .= pageListJSON]
    where
      pageListJSON =
        Ae.Array . V.fromList . fmap pageJSON $ pages
      pageJSON :: Page -> Ae.Value
      pageJSON Page {..} =
        Ae.object
          [ "title" .= pgmTitle pageMeta,
            "md_file" .= pageMdFile,
            "html_file" .= pageHtmlFile
          ]

data PageMeta = PageMeta
  { pgmId :: Text,
    pgmTitle :: Text,
    pgmToc :: Bool,
    pgmTocTitle :: Text,
    pgmTocDepth :: Int,
    pgmBibliography :: Maybe Ae.Value,
    pgmReferenceSectionTitle :: Text,
    pgmData :: Maybe Ae.Value
  }

instance Ae.FromJSON PageMeta where
  parseJSON = Ae.withObject "PageMeta" $ \v -> do
    PageMeta
      <$> v .: "id"
      <*> v .: "title"
      <*> v .:? "toc" .!= False
      <*> v .:? "toc-title" .!= defaultTocTitle
      <*> v .:? "toc-depth" .!= defaultTocDepth
      <*> v .:? "bibliography"
      <*> v .:? "reference-section-title" .!= defaultReferenceSectionTitle
      <*> v .:? "data"

instance Ae.ToJSON PageMeta where
  toJSON PageMeta {..} =
    Ae.object
      [ "id" .= pgmId,
        "title" .= pgmTitle,
        "toc" .= pgmToc,
        "toc-title" .= pgmTocTitle,
        "toc-depth" .= pgmTocDepth,
        "bibliography" .= pgmBibliography,
        "reference-section-title" .= pgmReferenceSectionTitle,
        "data" .= pgmData
      ]

data ListPage = ListPage
  { lTitle :: Text,
    lTemplate :: Mu.PName,
    lMeta :: Ae.Value,
    lData :: Ae.Value
  }

instance Ae.FromJSON ListPage where
  parseJSON = Ae.withObject "ListPage" $ \v ->
    ListPage
      <$> v .: "title"
      <*> fmap Mu.PName (v .: "template")
      <*> v .:? "meta" .!= Ae.Object mempty
      <*> v .: "data"

instance Ae.ToJSON ListPage where
  toJSON ListPage {..} = Ae.Object $ KM.union content meta
    where
      (Ae.Object content) =
        Ae.object
          [ "title" .= lTitle,
            "data" .= lData
          ]
      (Ae.Object meta) = Ae.toJSON lMeta

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

class IsMeta a where
  getTitle :: a -> Text
  getId :: a -> Text

  usesToc :: a -> Bool
  getTocTitle :: a -> Text
  getTocDepth :: a -> Int

  getBibliography :: a -> Maybe Ae.Value
  getReferenceSectionTitle :: a -> Text

  usesCitations :: a -> Bool
  usesCitations = isJust . getBibliography

  usesData :: a -> Bool
  usesData = isJust . getData

  getData :: a -> Maybe Ae.Value

instance IsMeta Meta where
  getTitle = mTitle
  getId = mId

  usesToc = mToc
  getTocTitle = mTocTitle
  getTocDepth = mTocDepth

  getBibliography = mBibliography
  getReferenceSectionTitle = mReferenceSectionTitle

  getData = mData

instance IsMeta PageMeta where
  getTitle = pgmTitle
  getId = pgmId

  usesToc = pgmToc
  getTocTitle = pgmTocTitle
  getTocDepth = pgmTocDepth

  getBibliography = pgmBibliography
  getReferenceSectionTitle = pgmReferenceSectionTitle

  getData = pgmData
