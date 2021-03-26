{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Peits.Pandoc where

import Control.Applicative ((<|>))
import Control.Monad (void)
import Control.Monad.Combinators (manyTill)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Ae
import qualified Data.Bifunctor as Bi
import Data.Foldable (foldl')
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (i, iii, __i)
import Data.Text (Text)
import qualified Data.Text as Tx
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.IO as Tx.IO
import Data.Void (Void)
import qualified Data.Yaml as Yaml
import qualified Development.Shake as S
import Development.Shake.FilePath ((-<.>))
import Network.URI (isRelativeReference)
import Peits.Config (Config (..), PandocMathMethod (..), SyntaxHighlightMethod (..))
import Peits.Env (Env (..), ShellCommand (..))
import Peits.Types
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M
import qualified Text.Pandoc as P
import Text.Pandoc.Builder (ToMetaValue)
import Text.Pandoc.Citeproc (processCitations)
import Text.Pandoc.Shared (addMetaField)
import qualified Text.Pandoc.Walk as PW

parseAndRenderPost ::
  Env -> FilePath -> Config -> S.Action Post
parseAndRenderPost env fp config = do
  (meta, html) <- parseAndRenderMd env fp config
  let htmlFp = "/" <> fp -<.> "html"
  pure $ Post meta fp htmlFp html

parseAndRenderPage ::
  Env -> FilePath -> Config -> S.Action Page
parseAndRenderPage env fp config = do
  (meta, html) <- parseAndRenderMd env fp config
  let htmlFp = "/" <> fp -<.> "html"
  pure $ Page meta fp htmlFp html

parseAndRenderMd ::
  (Ae.FromJSON meta, Toc meta, Bibliography meta) =>
  Env ->
  FilePath ->
  Config ->
  S.Action (meta, Html)
parseAndRenderMd env fp config = do
  (meta, _content, full) <- parseMd fp
  (meta,) <$> renderMd env meta full config

parseMd :: Ae.FromJSON meta => FilePath -> S.Action (meta, MdContent, MdFull)
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
  (Toc meta, Bibliography meta) =>
  Env ->
  meta ->
  MdFull ->
  Config ->
  S.Action Html
renderMd env meta (MdFull md) config = do
  let readerOptions = P.def {P.readerExtensions = P.pandocExtensions}
      mathMethod = case cPandocMathMethod config of
        MathJax -> P.MathJax ""
        Katex -> P.KaTeX ""
        MathML -> P.MathML
        PlainMath -> P.PlainMath
      tocTemplate =
        either error id $
          either (error . show) id $
            P.runPure $
              P.runWithDefaultPartials $
                P.compileTemplate "" (pandocTemplate (getTocTitle meta))
      writerOptions =
        P.def
          { P.writerExtensions = P.pandocExtensions,
            P.writerHTMLMathMethod = mathMethod,
            P.writerTemplate = Just tocTemplate,
            P.writerTableOfContents = usesToc meta,
            P.writerTOCDepth = getTocDepth meta
          }
      runPandoc p = liftIO $ P.handleError =<< P.runIO p

  doc <- runPandoc $ do
    mdDoc <- setRefSectionTitle meta <$> P.readMarkdown readerOptions md
    let filters = [processCitations | usesCitations meta]
    foldl' (>>=) (pure mdDoc) filters
  doc' <- filterPandoc env config doc

  Html <$> runPandoc (P.writeHtml5String writerOptions doc')

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

filterPandoc :: Env -> Config -> P.Pandoc -> S.Action P.Pandoc
filterPandoc env Config {..} = PW.walkM bfilter
  where
    bfilter :: P.Block -> S.Action P.Block
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

    ifilter :: P.Inline -> S.Action P.Inline
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

    pygments :: Text -> Maybe Text -> [Text] -> [(Text, Text)] -> S.Action Text
    pygments code mLang _clss _opts = do
      let lang = fromMaybe "text" mLang
          cmd =
            ShellCommand
              { shCommand = "pygmentize",
                shArgs = ["-l", lang, "-f", "html", "-O", "wrapcode"],
                shStdin = Just code,
                shExtra = Nothing
              }
      runShellCommand env cmd

    prismjs :: Text -> Maybe Text -> [Text] -> [(Text, Text)] -> Text
    prismjs code mLang clss opts =
      let lang = maybe "language-none" ("language-" `Tx.append`) mLang
          clss' = Tx.intercalate " " (lang : clss)
          hlLines, dataLine :: Text
          hlLines = fromMaybe "" (lookup "highlight" opts)
          dataLine = if Tx.null hlLines then "" else [i|data-line=#{hlLines}|]
       in [__i|<pre #{dataLine}><code class="#{clss'}">#{code}</code></pre>|]
