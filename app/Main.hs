{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Control.Applicative ((<|>))
import Control.Monad (forM, void)
import Control.Monad.Combinators (manyTill)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson ((.!=), (.:), (.:?), (.=))
import qualified Data.Aeson as Ae
import qualified Data.Bifunctor as Bi
import qualified Data.HashMap.Strict as HM
import Data.List (foldl', foldl1', sortOn)
import Data.Ord (Down (..))
import qualified Data.Set as Set
import Data.String.Interpolate.IsString (i)
import Data.Text (Text)
import qualified Data.Text as Tx
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.IO as Tx.IO
import qualified Data.Text.Lazy as Tx.L
import qualified Data.Text.Lazy.IO as Tx.L.IO
import qualified Data.Time.Clock as T
import qualified Data.Time.Format as TF
import qualified Data.Time.Format.ISO8601 as TF.Iso
import qualified Data.Vector as V
import Data.Void (Void)
import qualified Data.Yaml as Yaml
import Development.Shake ((%>))
import qualified Development.Shake as S
import Development.Shake.FilePath ((-<.>), (<.>), (</>))
import qualified Development.Shake.FilePath as SF
import System.Process (readProcess)
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M
import qualified Text.Mustache as Mu
import qualified Text.Pandoc as P
import qualified Text.Pandoc.Walk as PW

buildDir :: FilePath
buildDir = "_build"

getMatchingFiles :: S.FilePattern -> S.Action [FilePath]
getMatchingFiles = S.getDirectoryFiles "" . pure

newConstCache :: S.Action v -> S.Rules (S.Action v)
newConstCache action = fmap ($ ()) . S.newCache $ \() -> action

main :: IO ()
main = S.shakeArgs S.shakeOptions $ do
  S.phony "clean" $ do
    S.putNormal [i|Cleaning files in #{buildDir}|]
    S.removeFilesAfter buildDir ["//*"]
  config <- newConstCache $ do
    let configFile = "config.yml"
    S.need [configFile]
    liftIO (Yaml.decodeFileEither configFile) >>= \case
      Left err -> fail (Yaml.prettyPrintParseException err)
      Right c -> pure (c :: Config)
  templates <- newConstCache $ do
    let templateP = "templates/*.mustache"
    getMatchingFiles templateP >>= S.need
    liftIO (Mu.compileMustacheDir "default" (SF.takeDirectory templateP))
  -- get a post by file path
  getPost <- S.newCache $ \fp ->
    parseAndRenderPost fp
  -- collect all posts in a list, sorted in reverse chronological order
  allPosts <- newConstCache $ do
    files <- getMatchingFiles "posts/*.md"
    let sortFn = Down . mDate . postMeta
    fmap (sortOn sortFn) . forM files $ \fp -> do
      S.need [fp]
      getPost fp
  getPage <- S.newCache $ \fp ->
    parseAndRenderPage fp
  buildRoute postR $ \input output -> do
    cfg <- config
    ts <- templates
    S.need [input]
    post <- getPost input
    let content = unHtml $ postRendered post
        ctx = Ae.toJSON $ postMeta post
    renderTemplate
      cfg
      ts
      ["post", "default"]
      (Just content)
      [ctx]
      output
  buildRoute postListR $ \output -> do
    cfg <- config
    ts <- templates
    posts <- allPosts
    let tags = allTags posts
    S.need $ tagToPath <$> Set.toList tags
    let ctx = Ae.toJSON (PostList posts)
        ctxTitle = json "title" (Ae.String "Posts")
    renderTemplate
      cfg
      ts
      ["post_list", "default"]
      Nothing
      [ctxTitle, ctx]
      output
  buildRoute pageR $ \input output -> do
    cfg <- config
    ts <- templates
    S.need [input]
    page <- getPage input
    let content = unHtml $ pageRendered page
        ctx = Ae.toJSON $ pageMeta page
    renderTemplate
      cfg
      ts
      ["page", "default"]
      (Just content)
      [ctx]
      output
  buildRoute tagR $ \output -> do
    cfg <- config
    let tag = tagFromPath output
    ts <- templates
    posts <- filter (Set.member tag . mTags . postMeta) <$> allPosts
    let ctx = Ae.toJSON (PostList posts)
        title = [i|Posts tagged "#{unTag tag}"|]
        ctxTitle = json "title" (Ae.String title)
    renderTemplate
      cfg
      ts
      ["post_list_tag", "default"]
      Nothing
      [ctxTitle, ctx]
      output
  buildRoute tagListR $ \output -> do
    cfg <- config
    ts <- templates
    posts <- allPosts
    let tags = Set.unions $ map (mTags . postMeta) posts
        ctx = Ae.toJSON $ mkTagList tags
        ctxTitle = json "title" (Ae.String "Tags")
    renderTemplate
      cfg
      ts
      ["tag_list", "default"]
      Nothing
      [ctxTitle, ctx]
      output
  buildRoute projectListR $ \output -> do
    cfg <- config
    ts <- templates
    let ctxTitle = json "title" (Ae.String "Projects")
    renderTemplate
      cfg
      ts
      ["project_list", "default"]
      Nothing
      [ctxTitle]
      output
  buildRoute talkListR $ \output -> do
    cfg <- config
    ts <- templates
    let ctxTitle = json "title" (Ae.String "Talks")
    renderTemplate
      cfg
      ts
      ["talk_list", "default"]
      Nothing
      [ctxTitle]
      output
  buildRoute atomFeedR $ \output -> do
    cfg <- config
    ts <- templates
    posts <- allPosts
    let ctxPosts = Ae.toJSON (PostList posts)
        feedFile = Tx.pack (SF.dropDirectory1 output)
        feedUpdated = maximum $ fmap (mDate . postMeta) posts
        feed = Feed feedFile feedUpdated
        ctxFeed = Ae.toJSON feed
    renderTemplate
      cfg
      ts
      ["atom_feed"]
      Nothing
      [ctxFeed, ctxPosts]
      output
  buildRoute cssR S.copyFile'
  buildRoute imagesR S.copyFile'
  buildRoute keybaseR S.copyFile'
  -- buildRoute buildInfoR $ \output -> do
    -- let cmd = "git log -1 --format=%H%n%cd" :: String
    -- S.cmd (S.FileStdout output) cmd

tagFromPath :: FilePath -> Tag
tagFromPath =
  Tag
    . Tx.pack
    . SF.dropExtensions
    . SF.makeRelative (buildDir </> "tags")

tagToPath :: Tag -> FilePath
tagToPath (Tag tag) =
  buildDir </> "tags" </> Tx.unpack tag <.> "html"

allTags :: [Post] -> Set.Set Tag
allTags = foldl' Set.union mempty . fmap (mTags . postMeta)

type InputAndOutput = FilePath -> FilePath -> S.Action ()

type OnlyOutput = FilePath -> S.Action ()

data Route f where
  Mapping ::
    S.FilePattern ->
    (FilePath -> FilePath) ->
    Route InputAndOutput
  Generated ::
    S.FilePattern ->
    Route OnlyOutput
  Fixed ::
    FilePath ->
    Route OnlyOutput

buildRoute ::
  Route f ->
  f ->
  S.Rules ()
buildRoute (Mapping pat outf) f = do
  let mapOut x = buildDir </> outf x
  S.action $
    getMatchingFiles pat >>= S.need . fmap mapOut
  inputMap <- newConstCache $ do
    ifiles <- getMatchingFiles pat
    pure $ HM.fromList (zip (mapOut <$> ifiles) ifiles)
  mapOut pat %> \output -> do
    input <- (HM.! output) <$> inputMap
    f input output
buildRoute (Generated pat) f = do
  let outPat = buildDir </> pat
  outPat %> \output ->
    f output
buildRoute (Fixed outFile) f = do
  let outFile' = buildDir </> outFile
  S.want [outFile']
  outFile' %> \output ->
    f output

postR :: Route InputAndOutput
postR = Mapping "posts/*.md" (-<.> "html")

postListR :: Route OnlyOutput
postListR = Fixed "index.html"

pageR :: Route InputAndOutput
pageR = Mapping "pages/*.md" (-<.> "html")

cssR :: Route InputAndOutput
cssR = Mapping "static/css/*.css" SF.dropDirectory1

imagesR :: Route InputAndOutput
imagesR = Mapping "static/images/*" SF.dropDirectory1

keybaseR :: Route InputAndOutput
keybaseR = Mapping "static/keybase.txt" SF.dropDirectory1

buildInfoR :: Route OnlyOutput
buildInfoR = Fixed "build-info.txt"

tagR :: Route OnlyOutput
tagR = Generated "tags/*.html"

tagListR :: Route OnlyOutput
tagListR = Fixed "tags.html"

projectListR :: Route OnlyOutput
projectListR = Fixed "projects.html"

talkListR :: Route OnlyOutput
talkListR = Fixed "talks.html"

atomFeedR :: Route OnlyOutput
atomFeedR = Fixed "atom.xml"

parseAndRenderPost :: (MonadIO m, MonadFail m) => FilePath -> m Post
parseAndRenderPost fp = do
  (meta, html) <- parseAndRenderMd fp
  let htmlFp = "/" <> fp -<.> "html"
  pure $ Post meta fp htmlFp html

parseAndRenderPage :: (MonadIO m, MonadFail m) => FilePath -> m Page
parseAndRenderPage fp = do
  (meta, html) <- parseAndRenderMd fp
  let htmlFp = "/" <> fp -<.> "html"
  pure $ Page meta fp htmlFp html

parseAndRenderMd :: (MonadIO m, MonadFail m, Ae.FromJSON meta) => FilePath -> m (meta, Html)
parseAndRenderMd fp = do
  (meta, md) <- parseMd fp
  (meta,) <$> renderMd md

renderTemplate ::
  MonadIO m =>
  Config ->
  Mu.Template ->
  [Mu.PName] ->
  -- | content to interpolate
  Maybe Text ->
  -- | context values
  [Ae.Value] ->
  FilePath ->
  m ()
renderTemplate cfg allTmpls tmpls mcontent ctx out =
  liftIO . Tx.L.IO.writeFile out $
    foldl f (maybe mempty Tx.L.fromStrict mcontent) tmpls
  where
    f :: Tx.L.Text -> Mu.PName -> Tx.L.Text
    f content tmpl =
      Mu.renderMustache
        (getTmpl tmpl)
        (mergeJSON (Ae.toJSON cfg : json "content" content : ctx))
    getTmpl tmpl =
      allTmpls {Mu.templateActual = tmpl}

json :: Ae.ToJSON v => Text -> v -> Ae.Value
json k v = Ae.object [k .= Ae.toJSON v]

mergeJSON :: [Ae.Value] -> Ae.Value
mergeJSON = foldl1' merge
  where
    merge (Ae.Object l) (Ae.Object r) = Ae.Object (HM.union l r)
    merge _ _ = error "Cannot merge non-object values"

parseMd :: (MonadIO m, MonadFail m, Ae.FromJSON meta) => FilePath -> m (meta, Md)
parseMd fp = do
  contents <- liftIO $ Tx.IO.readFile fp
  (metaStr, md) <- case partitionMd fp contents of
    Left err -> fail err
    Right (Nothing, _) -> fail [i|No metadata block in #{fp}|]
    Right (Just m, c) -> pure $ Bi.first encodeUtf8 (m, c)
  meta <- liftIO $ Yaml.decodeThrow metaStr
  pure (meta, Md md)

renderMd :: MonadIO m => Md -> m Html
renderMd (Md md) = liftIO $ do
  let readerOptions = P.def {P.readerExtensions = P.pandocExtensions}
  let writerOptions = P.def {P.writerHTMLMathMethod = P.MathJax ""}
  result <- P.runIO $ do
    doc <- P.readMarkdown readerOptions md
    doc' <- liftIO $ pygmentize doc
    P.writeHtml5String writerOptions doc'
  Html <$> P.handleError result

test :: IO ()
test = Yaml.decodeThrow ""

-- stolen from https://github.com/srid/neuron
partitionMd :: FilePath -> Text -> Either String (Maybe Text, Text)
partitionMd fp md = Bi.first M.errorBundlePretty (M.parse parser fp md)
  where
    parser =
      (M.try splitP <|> fmap (Nothing,) M.takeRest) <* M.eof
    separatorP :: M.Parsec Void Text ()
    separatorP =
      void $ M.string "---" <* M.eol
    splitP :: M.Parsec Void Text (Maybe Text, Text)
    splitP = do
      separatorP
      a <- Tx.pack <$> manyTill M.anySingle (M.try $ M.eol *> separatorP)
      b <- M.takeRest
      pure (Just a, b)

pygmentize :: P.Pandoc -> IO P.Pandoc
pygmentize = PW.walkM pfilter
  where
    pfilter :: P.Block -> IO P.Block
    pfilter = \case
      (P.CodeBlock (_ids, clss, options) code) ->
        P.RawBlock (P.Format "html") <$> pygments code clss options
      other -> pure other
    pygments :: Text -> [Text] -> [(Text, Text)] -> IO Text
    pygments code clss _opts = do
      let (lang, _classes) = case clss of
            [] -> (Nothing, [])
            (l : rest) -> (Just l, rest)
          langArgs = case lang of
            Nothing -> []
            Just l -> ["-l", l]
          args = Tx.unpack <$> langArgs <> ["-f", "html"]
      Tx.pack <$> readProcess "pygmentize" args (Tx.unpack code)

parseDate :: DateFmt -> Text -> Maybe T.UTCTime
parseDate (DateFmt fmt) datestr =
  TF.parseTimeM True TF.defaultTimeLocale (Tx.unpack fmt) (Tx.unpack datestr)

showDate :: DateFmt -> T.UTCTime -> Text
showDate (DateFmt fmt) date =
  Tx.pack $ TF.formatTime TF.defaultTimeLocale (Tx.unpack fmt) date

showDateIso :: T.UTCTime -> Text
showDateIso = Tx.pack . TF.Iso.iso8601Show

-- * Constants

-- | 2020-01-02
postDateReadFmt :: DateFmt
postDateReadFmt = DateFmt "%0Y-%m-%d"

-- | January 2, 2020
postDateShowLongFmt :: DateFmt
postDateShowLongFmt = DateFmt "%B %e,%0Y"

-- | 2020.01.02
postDateShowShortFmt :: DateFmt
postDateShowShortFmt = DateFmt "%0Y.%m.%d"

-- * Types

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

data Meta = Meta
  { mTitle :: Text,
    mDate :: T.UTCTime,
    mTags :: Set.Set Tag,
    mDesc :: Maybe Text
  }
  deriving (Show)

instance Ae.FromJSON Meta where
  parseJSON = Ae.withObject "Meta" $ \v -> do
    title <- v .: "title"
    dateStr <- v .: "date"
    date <- case parseDate postDateReadFmt dateStr of
      Just d -> pure $ d
      Nothing ->
        fail [i|Cannot parse date "#{dateStr}" for post titled "#{title}"|]
    tags <- v .:? "tags" .!= mempty
    desc <- v .:? "description"
    pure $ Meta title date tags desc

instance Ae.ToJSON Meta where
  toJSON Meta {..} =
    Ae.object
      [ "title" .= mTitle,
        "date_long" .= showDate postDateShowLongFmt mDate,
        "date_short" .= showDate postDateShowShortFmt mDate,
        "date_iso" .= showDateIso mDate,
        "tags" .= Set.map TagLink mTags,
        "description" .= mDesc
      ]

data Page = Page
  { pageMeta :: PageMeta,
    pageMdFile :: FilePath,
    pageHtmlFile :: FilePath,
    pageRendered :: Html
  }

data PageMeta = PageMeta
  { pageMetaTitle :: Text
  }

instance Ae.FromJSON PageMeta where
  parseJSON = Ae.withObject "PageMeta" $ \v -> do
    PageMeta <$> v .: "title"

instance Ae.ToJSON PageMeta where
  toJSON PageMeta {..} =
    Ae.object
      [ "title" .= pageMetaTitle
      ]

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

newtype Md = Md {unMd :: Text}

newtype Html = Html {unHtml :: Text}
  deriving (Show)

newtype DateFmt = DateFmt {unDateFmt :: Text}

data Config = Config
  { cSiteTitle :: Text,
    cAuthor :: Text,
    cEmail :: Text,
    cCopyright :: Text,
    cHost :: Text,
    cProjects :: [Project],
    cTalks :: [Talk]
  }

instance Ae.FromJSON Config where
  parseJSON = Ae.withObject "Config" $ \v ->
    Config
      <$> v .: "site_title"
      <*> v .: "author"
      <*> v .: "email"
      <*> v .: "copyright"
      <*> v .: "host"
      <*> v .:? "projects" .!= []
      <*> v .:? "talks" .!= []

instance Ae.ToJSON Config where
  toJSON Config {..} =
    Ae.object
      [ "site_title" .= cSiteTitle,
        "author" .= cAuthor,
        "email" .= cEmail,
        "copyright" .= cCopyright,
        "host" .= cHost,
        "projects" .= cProjects,
        "talks" .= cTalks
      ]

data Project = Project
  { prName :: Text,
    prLink :: Text,
    prDesc :: Text,
    prDocs :: Maybe Text
  }

instance Ae.FromJSON Project where
  parseJSON = Ae.withObject "Project" $ \v ->
    Project
      <$> v .: "name"
      <*> v .: "link"
      <*> v .: "description"
      <*> v .:? "docs" .!= Nothing

instance Ae.ToJSON Project where
  toJSON Project {..} =
    Ae.object
      [ "name" .= prName,
        "link" .= prLink,
        "description" .= prDesc,
        "docs" .= prDocs
      ]

data Talk = Talk
  { tkName :: Text,
    tkLink :: Text,
    tkDate :: Text,
    tkLocation :: Text,
    tkLocationLink :: Text
  }

instance Ae.FromJSON Talk where
  parseJSON = Ae.withObject "Talk" $ \v ->
    Talk
      <$> v .: "name"
      <*> v .: "link"
      <*> v .: "date"
      <*> v .: "location"
      <*> v .: "location_link"

instance Ae.ToJSON Talk where
  toJSON Talk {..} =
    Ae.object
      [ "name" .= tkName,
        "link" .= tkLink,
        "date" .= tkDate,
        "location" .= tkLocation,
        "location_link" .= tkLocationLink
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
