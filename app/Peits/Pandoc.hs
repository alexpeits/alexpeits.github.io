{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Peits.Pandoc where

import Control.Applicative ((<|>))
import Control.Monad (void)
import Control.Monad.Combinators (manyTill)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Aeson as Ae
import qualified Data.Bifunctor as Bi
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (i, iii, __i)
import Data.Text (Text)
import qualified Data.Text as Tx
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.IO as Tx.IO
import Data.Void (Void)
import qualified Data.Yaml as Yaml
import Development.Shake.FilePath ((-<.>))
import Network.URI (isRelativeReference)
import Peits.Types
import System.Process (readProcess)
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M
import qualified Text.Pandoc as P
import Text.Pandoc.Builder (ToMetaValue)
import Text.Pandoc.Citeproc (processCitations)
import Text.Pandoc.Shared (addMetaField)
import qualified Text.Pandoc.Walk as PW

parseAndRenderPost ::
  (MonadIO m, MonadFail m) => FilePath -> Config -> m Post
parseAndRenderPost fp config = do
  (meta, html) <- parseAndRenderMd fp config
  let htmlFp = "/" <> fp -<.> "html"
  pure $ Post meta fp htmlFp html

parseAndRenderPage ::
  (MonadIO m, MonadFail m) => FilePath -> Config -> m Page
parseAndRenderPage fp config = do
  (meta, html) <- parseAndRenderMd fp config
  let htmlFp = "/" <> fp -<.> "html"
  pure $ Page meta fp htmlFp html

parseAndRenderMd ::
  (MonadIO m, MonadFail m, Ae.FromJSON meta, Toc meta, Bibliography meta) =>
  FilePath ->
  Config ->
  m (meta, Html)
parseAndRenderMd fp config = do
  (meta, _content, full) <- parseMd fp
  (meta,) <$> renderMd meta full config

parseMd :: (MonadIO m, MonadFail m, Ae.FromJSON meta) => FilePath -> m (meta, MdContent, MdFull)
parseMd fp = do
  mdFull <- liftIO $ Tx.IO.readFile fp
  (metaStr, md) <- case partitionMd fp mdFull of
    Left err -> fail err
    Right (Nothing, _) -> fail [i|No metadata block in #{fp}|]
    Right (Just m, c) -> pure $ Bi.first encodeUtf8 (m, c)
  meta <- liftIO $ Yaml.decodeThrow metaStr
  pure (meta, MdContent md, MdFull mdFull)

pandocTemplate :: Text -> Text
pandocTemplate tocTitle =
  [iii|
    $if(toc)$
    <div id="toc">
      <div class="toc-title">
        #{tocTitle}
      </div>
      <div class="toc-contents">
        $table-of-contents$
      </div>
    </div>
    $endif$
    <div id="main">
      $body$
    </div>
    |]

renderMd ::
  (MonadIO m, Toc meta, Bibliography meta) => meta -> MdFull -> Config -> m Html
renderMd meta (MdFull md) config = liftIO $ do
  let readerOptions = P.def {P.readerExtensions = P.pandocExtensions}
      tocTemplate =
        either error id $
          either (error . show) id $
            P.runPure $
              P.runWithDefaultPartials $
                P.compileTemplate "" (pandocTemplate (getTocTitle meta))
      writerOptions =
        P.def
          { P.writerExtensions = P.pandocExtensions,
            P.writerHTMLMathMethod = P.MathJax "",
            P.writerTemplate = Just tocTemplate,
            P.writerTableOfContents = usesToc meta,
            P.writerTOCDepth = getTocDepth meta
          }
  result <- P.runIO $ do
    doc <- setRefSectionTitle meta <$> P.readMarkdown readerOptions md
    let citeFilters = [processCitations | usesCitations meta]
        otherFilters = citeFilters
        filters = (liftIO . filterPandoc config) : otherFilters
    doc' <- foldl (>>=) (pure doc) filters
    P.writeHtml5String writerOptions doc'
  Html <$> P.handleError result

setRefSectionTitle :: Bibliography meta => meta -> P.Pandoc -> P.Pandoc
setRefSectionTitle meta (P.Pandoc pMeta blocks) = P.Pandoc pMeta' blocks
  where
    refSectionTitle = getReferenceSectionTitle meta
    pMeta' = maybeAddMeta "reference-section-title" refSectionTitle pMeta

maybeAddMeta :: ToMetaValue a => Text -> a -> P.Meta -> P.Meta
maybeAddMeta k v m =
  case P.lookupMeta k m of
    Nothing -> addMetaField k v m
    Just _ -> m

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

filterPandoc :: Config -> P.Pandoc -> IO P.Pandoc
filterPandoc Config {..} = PW.walkM bfilter
  where
    bfilter :: P.Block -> IO P.Block
    bfilter = \case
      cb@(P.CodeBlock (_ids, clss, options) code) ->
        let (lang, classes) = splitCodeClasses clss
         in case cSyntaxHighlightMethod of
              Default ->
                pure $
                  P.Div ("", ["skylighting"], []) [cb]
              Pygments ->
                P.RawBlock (P.Format "html") <$> pygments code lang classes options
              PrismJS ->
                pure $ P.RawBlock (P.Format "html") (prismjs code lang classes options)
      other -> PW.walkM ifilter other

    ifilter :: P.Inline -> IO P.Inline
    ifilter = \case
      (P.Link (ids, clss, options) inlines tgt@(uri, _title)) -> do
        let newClss =
              if isRelativeReference (Tx.unpack uri)
                then "internal-link" : clss
                else clss
        pure $ P.Link (ids, newClss, options) inlines tgt
      other -> pure other

    splitCodeClasses :: [Text] -> (Maybe Text, [Text])
    splitCodeClasses = \case
      [] -> (Nothing, [])
      (l : rest) -> (Just l, rest)

    pygments :: Text -> Maybe Text -> [Text] -> [(Text, Text)] -> IO Text
    pygments code mLang _clss _opts = do
      let lang = fromMaybe "text" mLang
          args = Tx.unpack <$> ["-l", lang, "-f", "html"]
      Tx.pack <$> readProcess "pygmentize" args (Tx.unpack code)

    prismjs :: Text -> Maybe Text -> [Text] -> [(Text, Text)] -> Text
    prismjs code mLang clss opts =
      let lang = maybe "language-none" ("language-" `Tx.append`) mLang
          clss' = Tx.intercalate " " (lang:clss)
          hlLines, dataLine :: Text
          hlLines = fromMaybe "" (lookup "highlight" opts)
          dataLine = if Tx.null hlLines then "" else [i|data-line=#{hlLines}|]
       in [__i|<pre #{dataLine}><code class="#{clss'}">#{code}</code></pre>|]
