{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Peits.Config where

import Data.Aeson ((.!=), (.:), (.:?), (.=))
import qualified Data.Aeson as Ae
import Data.String.Interpolate (i)
import Data.Text (Text)

data Config = Config
  { cSiteTitle :: Text,
    cAuthor :: Text,
    cEmail :: Text,
    cCopyright :: Text,
    cHost :: Text,
    cSyntaxHighlightMethod :: SyntaxHighlightMethod,
    cPandocMathMethod :: PandocMathMethod,
    cMermaid :: MermaidConfig,
    cNav :: [NavItem],
    cRaw :: Ae.Value
  }

instance Ae.FromJSON Config where
  parseJSON = Ae.withObject "Config" $ \v ->
    Config
      <$> v .: "site_title"
      <*> v .: "author"
      <*> v .: "email"
      <*> v .: "copyright"
      <*> v .: "host"
      <*> v .:? "syntax_highlight" .!= Pygments
      <*> v .:? "math" .!= MathJax
      <*> v .:? "mermaid" .!= defaultMermaidConfig
      <*> v .: "nav"
      <*> pure (Ae.Object v)

instance Ae.ToJSON Config where
  toJSON Config {..} =
    Ae.object
      [ "site_title" .= cSiteTitle,
        "author" .= cAuthor,
        "email" .= cEmail,
        "copyright" .= cCopyright,
        "syntax_highlight" .= ("" :: Text),
        "highlight_default" .= (cSyntaxHighlightMethod == Default),
        "highlight_pygments" .= (cSyntaxHighlightMethod == Pygments),
        "highlight_prismjs" .= (cSyntaxHighlightMethod == PrismJS),
        "host" .= cHost,
        "nav" .= cNav
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

data SyntaxHighlightMethod
  = Default
  | Pygments
  | PrismJS
  deriving (Eq, Show)

instance Ae.FromJSON SyntaxHighlightMethod where
  parseJSON = Ae.withText "SyntaxHighlightMethod" $ \case
    "default" -> pure Default
    "pygments" -> pure Pygments
    "prismjs" -> pure PrismJS
    other -> fail [i|Cannot parse syntax highlight method "#{other}"|]

data PandocMathMethod
  = MathJax
  | Katex
  | MathML
  | PlainMath

instance Ae.FromJSON PandocMathMethod where
  parseJSON = Ae.withText "PandocMathMethod" $ \case
    "mathjax" -> pure MathJax
    "katex" -> pure Katex
    "mathml" -> pure MathML
    "default" -> pure PlainMath
    other -> fail [i|Cannot parse math method "#{other}"|]

data MermaidConfig = MermaidConfig
  { mcExt :: MermaidExt,
    mcLightTheme :: MermaidTheme,
    mcDarkTheme :: MermaidTheme
  }

defaultMermaidConfig :: MermaidConfig
defaultMermaidConfig =
  MermaidConfig
    { mcExt = MermaidSvg,
      mcLightTheme = DefaultTheme,
      mcDarkTheme = DarkTheme
    }

instance Ae.FromJSON MermaidConfig where
  parseJSON = Ae.withObject "MermaidConfig" $ \v ->
    MermaidConfig
      <$> v .:? "ext" .!= MermaidSvg
      <*> v .:? "light_theme" .!= DefaultTheme
      <*> v .:? "dark_theme" .!= DarkTheme

data MermaidExt
  = MermaidSvg
  | MermaidPng

instance Ae.FromJSON MermaidExt where
  parseJSON = Ae.withText "MermaidExt" $ \case
    "svg" -> pure MermaidSvg
    "png" -> pure MermaidPng
    other -> fail [i|Cannot parse mermaid output extension "#{other}"|]

data MermaidTheme
  = DefaultTheme
  | ForestTheme
  | NeutralTheme
  | DarkTheme

instance Ae.FromJSON MermaidTheme where
  parseJSON = Ae.withText "MermaidTheme" $ \case
    "default" -> pure DefaultTheme
    "forest" -> pure ForestTheme
    "neutral" -> pure NeutralTheme
    "dark" -> pure DarkTheme
    other -> fail [i|Cannot parse mermaid theme "#{other}"|]
