{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Ae
import Data.List (foldl', sortOn)
import Data.Ord (Down (..))
import qualified Data.Set as Set
import Data.String.Interpolate (i)
import qualified Data.Text as Tx
import qualified Development.Shake as S
import Development.Shake.FilePath ((<.>), (</>))
import qualified Development.Shake.FilePath as SF
import Peits.Config (Config (..), MermaidConfig (..), MermaidTheme (..))
import Peits.Constants (buildDir, configFile)
import Peits.Env (Env (..), ShellCommand (..))
import Peits.Options (Options (..), getOptions, options)
import Peits.Pandoc (parseAndRenderPage, parseAndRenderPost)
import Peits.Routes
import Peits.Template (renderTemplate)
import Peits.Types
import Peits.Util (getMatchingFiles, json, newConstCache, readYaml)
import System.Process (readProcess)
import qualified Text.Mustache as Mu

--- $> :main clean

--- $> :main

main :: IO ()
main = S.shakeArgsWith S.shakeOptions options $ \flags _targets -> (pure . Just) $ do
  let opts = getOptions flags

  S.phony "clean" $ do
    S.putNormal [i|Cleaning files in #{buildDir}|]
    S.removeFilesAfter buildDir ["//*"]

  runSh <- S.addOracleCache $ \ShellCommand {..} -> do
    let cmd = Tx.unpack shCommand
        args = Tx.unpack <$> shArgs
        stdin = maybe mempty Tx.unpack shStdin
    liftIO $ Tx.pack <$> readProcess cmd args stdin

  let env = Env {runShellCommand = runSh}

  let readConfig :: IO Config
      readConfig = do
        conf <- readYaml configFile
        pure $ case optSyntaxHighlightMethod opts of
          Nothing -> conf
          Just meth -> conf {cSyntaxHighlightMethod = meth}

  config <- newConstCache $ do
    S.need [configFile]
    liftIO readConfig

  templates <- newConstCache $ do
    let templateP = "templates/*.mustache"
    getMatchingFiles templateP >>= S.need
    liftIO (Mu.compileMustacheDir "default" (SF.takeDirectory templateP))

  -- get a post by file path
  getPost <- S.newCache $ \fp -> do
    S.need [fp]
    cfg <- config
    parseAndRenderPost env fp cfg
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
    parseAndRenderPage env fp cfg

  getList <- S.newCache $ \fp -> do
    S.need [fp]
    liftIO $ readYaml fp

  let buildPage input output = case SF.takeExtension input of
        ".md" -> buildMdPage input output
        ".yml" -> buildYmlPage input output
        ".yaml" -> buildYmlPage input output
        other -> fail [i|Unrecognized extension #{other} in pages/|]
      buildMdPage input output = do
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
      buildYmlPage input output = do
        cfg <- config
        ts <- templates
        list <- getList input
        let template = lTemplate list
            ctxTitle = json "title" $ Ae.String (lTitle list)
            ctx = Ae.toJSON list
        renderTemplate
          cfg
          ts
          [template, "default"]
          Nothing
          [ctxTitle, ctx]
          output

  buildRoute pageR buildPage

  buildRoute draftR buildMdPage

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
    ts <- templates
    let tag = tagFromPath output
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

  let buildMermaid theme = \output -> do
        mCfg <- cMermaid <$> config
        let subdir = case theme of
              Light -> "light"
              Dark -> "dark"
            mTheme = mermaidThemeCli $ case theme of
              Light -> mcLightTheme mCfg
              Dark -> mcDarkTheme mCfg
            name =
              SF.dropExtensions $
                SF.makeRelative (buildDir </> "assets" </> "mermaid" </> subdir) output
            input = "tmp" </> name <.> "mmd"
        S.need [input]
        S.command_
          []
          "mmdc"
          ["-i", input, "-o", output, "--theme", mTheme, "-b", "transparent"]

  buildRoute mermaidDiagramLightR $ buildMermaid Light
  buildRoute mermaidDiagramDarkR $ buildMermaid Dark

  buildRoute imagesR S.copyFile'
  buildRoute keybaseR S.copyFile'

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

mermaidThemeCli :: MermaidTheme -> String
mermaidThemeCli = \case
  DefaultTheme -> "default"
  ForestTheme -> "forest"
  NeutralTheme -> "neutral"
  DarkTheme -> "dark"
