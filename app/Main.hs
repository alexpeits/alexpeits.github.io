{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Ae
import Data.Function ((&))
import qualified Data.HashMap.Strict as HM
import Data.List (foldl', sortOn)
import qualified Data.Map.Strict as Map
import Data.Ord (Down (..))
import qualified Data.Set as Set
import Data.String.Interpolate (i)
import Data.Text (Text)
import qualified Data.Text as Tx
import qualified Data.Yaml as Yaml
import Development.Shake ((%>))
import qualified Development.Shake as S
import Development.Shake.FilePath ((-<.>), (<.>), (</>))
import qualified Development.Shake.FilePath as SF
import Peits.Pandoc
import Peits.Types
import Peits.Util
import System.Console.GetOpt (ArgDescr (..), OptDescr (..))
import qualified Text.Mustache as Mu

buildDir :: FilePath
buildDir = "_build"

--- $> :main clean

--- $> :main

data Options = Options
  { optSyntaxHighlightMethod :: Maybe SyntaxHighlightMethod
  }

defaultOptions :: Options
defaultOptions =
  Options
    { optSyntaxHighlightMethod = Nothing
    }

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
      Just x -> Left $ "Unknown syntax highlighting method " ++ x

shakeArgs :: [OptDescr (Either String (Options -> Options))]
shakeArgs =
  [ Option
      ""
      ["highlight"]
      (OptArg readSyntaxHighlightMethod "METHOD")
      "Syntax highlighting method (default/pygments/prismjs)"
  ]

main :: IO ()
main = S.shakeArgsWith S.shakeOptions shakeArgs $ \flags _targets -> (pure . Just) $ do
  let Options {..} = foldl (&) defaultOptions flags

  S.phony "clean" $ do
    S.putNormal [i|Cleaning files in #{buildDir}|]
    S.removeFilesAfter buildDir ["//*"]

  let configFile = "config.yml"
      readConfig :: IO Config
      readConfig = do
        conf <-
          Yaml.decodeFileEither configFile >>= \case
            Left err -> fail (Yaml.prettyPrintParseException err)
            Right c -> pure (c :: Config)
        pure $ case optSyntaxHighlightMethod of
          Nothing -> conf
          Just meth -> conf {cSyntaxHighlightMethod = meth}

  config <- newConstCache $ do
    S.need [configFile]
    liftIO readConfig

  configNoDep <- liftIO readConfig

  templates <- newConstCache $ do
    let templateP = "templates/*.mustache"
    getMatchingFiles templateP >>= S.need
    liftIO (Mu.compileMustacheDir "default" (SF.takeDirectory templateP))

  -- get a post by file path
  getPost <- S.newCache $ \fp -> do
    S.need [fp]
    cfg <- config
    parseAndRenderPost fp cfg
  -- collect all posts in a list, sorted in reverse chronological order
  allPosts <- newConstCache $ do
    files <- getMatchingFiles "posts/*.md"
    let sortFn = Down . mDate . postMeta
    fmap (sortOn sortFn) . forM files $ \fp -> do
      getPost fp
  buildRoute postR $ \input output -> do
    cfg <- config
    ts <- templates
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

  getPage <- S.newCache $ \fp -> do
    S.need [fp]
    cfg <- config
    parseAndRenderPage fp cfg

  let buildPage input output = do
        cfg <- config
        ts <- templates
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

  buildRoute pageR buildPage

  buildRoute draftR buildPage

  allDrafts <- newConstCache $ do
    files <- getMatchingFiles "drafts/*.md"
    forM files getPage

  buildRoute draftListR $ \output -> do
    cfg <- config
    ts <- templates
    drafts <- allDrafts
    let ctx = Ae.toJSON (PageList drafts)
        ctxTitle = json "title" (Ae.String "Drafts")
    renderTemplate
      cfg
      ts
      ["draft_list", "default"]
      Nothing
      [ctxTitle, ctx]
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

  -- gather all lists to generate
  let lists = Map.keys (cLists configNoDep)
      listFile p = buildDir </> "lists" </> p <.> "html"
      listFiles = fmap (listFile . Tx.unpack) lists
  S.want listFiles

  buildRoute listR $ \output -> do
    cfg <- config
    ts <- templates
    let name = listNameFromPath output
        listCfg = cLists cfg Map.! name
        template = lTemplate listCfg
        title = lTitle listCfg
        ctxTitle = json "title" (Ae.String title)
        ctx = Ae.toJSON listCfg
    renderTemplate
      cfg
      ts
      [template, "default"]
      Nothing
      [ctxTitle, ctx]
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

  let minify input output = do
        S.need [input]
        let cmd :: [String]
            cmd = ["minify", "-o", output, input]
        S.cmd_ cmd

  buildRoute cssR minify
  buildRoute jsR minify

  buildRoute imagesR S.copyFile'
  buildRoute keybaseR S.copyFile'

-- routes

type InputAndOutput = FilePath -> FilePath -> S.Action ()

type OnlyOutput = FilePath -> S.Action ()

data Route f where
  Mapping :: S.FilePattern -> (FilePath -> FilePath) -> Route InputAndOutput
  Generated :: S.FilePattern -> Route OnlyOutput
  Fixed :: FilePath -> Route OnlyOutput

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

draftR :: Route InputAndOutput
draftR = Mapping "drafts/*.md" (-<.> "html")

draftListR :: Route OnlyOutput
draftListR = Fixed "drafts.html"

cssR :: Route InputAndOutput
cssR = Mapping "static/css/*.css" SF.dropDirectory1

jsR :: Route InputAndOutput
jsR = Mapping "static/js/*.js" SF.dropDirectory1

imagesR :: Route InputAndOutput
imagesR = Mapping "static/images/*" SF.dropDirectory1

keybaseR :: Route InputAndOutput
keybaseR = Mapping "static/keybase.txt" SF.dropDirectory1

tagR :: Route OnlyOutput
tagR = Generated "tags/*.html"

tagListR :: Route OnlyOutput
tagListR = Fixed "tags.html"

listR :: Route OnlyOutput
listR = Generated "lists/*.html"

atomFeedR :: Route OnlyOutput
atomFeedR = Fixed "atom.xml"

-- helpers

allTags :: [Post] -> Set.Set Tag
allTags = foldl' Set.union mempty . fmap (mTags . postMeta)

tagToPath :: Tag -> FilePath
tagToPath (Tag tag) =
  buildDir </> "tags" </> Tx.unpack tag <.> "html"

tagFromPath :: FilePath -> Tag
tagFromPath =
  Tag
    . Tx.pack
    . SF.dropExtensions
    . SF.makeRelative (buildDir </> "tags")

listNameFromPath :: FilePath -> Text
listNameFromPath =
  Tx.pack
    . SF.dropExtensions
    . SF.makeRelative (buildDir </> "lists")

newConstCache :: S.Action v -> S.Rules (S.Action v)
newConstCache action = fmap ($ ()) . S.newCache $ \() -> action
