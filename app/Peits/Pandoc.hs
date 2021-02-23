{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}

module Peits.Pandoc where

import Control.Applicative ((<|>))
import Control.Monad (void)
import Control.Monad.Combinators (manyTill)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Control.Monad.State.Strict as St
import qualified Data.Aeson as Ae
import qualified Data.Bifunctor as Bi
import Data.String.Interpolate.IsString (i)
import Data.Text (Text)
import qualified Data.Text as Tx
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.IO as Tx.IO
import Data.Void (Void)
import qualified Data.Yaml as Yaml
import Development.Shake.FilePath ((-<.>))
import Peits.Types
import System.Process (readProcess)
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M
import qualified Text.Pandoc as P
import qualified Text.Pandoc.Walk as PW

parseAndRenderPost :: (MonadIO m, MonadFail m) => FilePath -> m Post
parseAndRenderPost fp = do
  (meta, html) <- parseAndRenderMd fp
  let htmlFp = "/" <> fp -<.> "html"
  pure $ Post meta fp htmlFp html

renderPost :: MonadIO m => FilePath -> Meta -> MdFull -> m Post
renderPost fp meta full = do
  html <- renderMd meta full
  let htmlFp = "/" <> fp -<.> "html"
  pure $ Post meta fp htmlFp html

parseAndRenderPage :: (MonadIO m, MonadFail m) => FilePath -> m Page
parseAndRenderPage fp = do
  (meta, html) <- parseAndRenderMd fp
  let htmlFp = "/" <> fp -<.> "html"
  pure $ Page meta fp htmlFp html

parseAndRenderMd ::
  (MonadIO m, MonadFail m, Ae.FromJSON meta, Toc meta) =>
  FilePath ->
  m (meta, Html)
parseAndRenderMd fp = do
  (meta, _content, full) <- parseMd fp
  (meta,) <$> renderMd meta full

parseMd :: (MonadIO m, MonadFail m, Ae.FromJSON meta) => FilePath -> m (meta, MdContent, MdFull)
parseMd fp = do
  mdFull <- liftIO $ Tx.IO.readFile fp
  (metaStr, md) <- case partitionMd fp mdFull of
    Left err -> fail err
    Right (Nothing, _) -> fail [i|No metadata block in #{fp}|]
    Right (Just m, c) -> pure $ Bi.first encodeUtf8 (m, c)
  meta <- liftIO $ Yaml.decodeThrow metaStr
  pure (meta, MdContent md, MdFull mdFull)

renderMd :: (MonadIO m, Toc meta) => meta -> MdFull -> m Html
renderMd meta (MdFull md) = liftIO $ do
  let readerOptions = P.def {P.readerExtensions = P.pandocExtensions}
      tocTemplate =
        either error id $
          either (error . show) id $
            P.runPure $
              P.runWithDefaultPartials $
                P.compileTemplate "" "<div id=\"toc\">$toc$</div>\n<div id=\"main\">$body$</div>"
      writerOptions =
        P.def
          { P.writerExtensions = P.pandocExtensions,
            P.writerHTMLMathMethod = P.MathJax "",
            P.writerTemplate = Just tocTemplate,
            P.writerTableOfContents = renderToc meta,
            P.writerTOCDepth = tocDepth meta
          }
  result <- P.runIO $ do
    doc <- P.readMarkdown readerOptions md
    doc' <- liftIO $ pygmentize doc
    (doc'', x) <- liftIO $ St.runStateT (processLinks doc') []
    liftIO $ print x
    P.writeHtml5String writerOptions doc''
  Html <$> P.handleError result

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

processLinks :: P.Pandoc -> St.StateT [Text] IO P.Pandoc
processLinks = PW.walkM pfilter
  where
    pfilter :: P.Inline -> St.StateT [Text] IO P.Inline
    pfilter = \case
      (P.Link (ids, clss, options) inlines t@(url, _title)) -> do
        St.modify (url :)
        pure $ P.Link (ids, "foo" : clss, options) inlines t
      a@(P.RawInline "html" txt) -> case Tx.stripPrefix "<id:" txt of
        Nothing -> pure a
        Just url -> do
          St.modify (url :)
          pure $ P.Link ("", ["foo"], []) [] ("foo", "bar")
      other -> pure other
