{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Peits.Html where

import Data.String.Interpolate (iii)
import Data.Text (Text)

layout :: Text -> Text
layout tocTitle =
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

picture :: Bool -> FilePath -> FilePath -> Text
picture wide light dark =
  [iii|
    <picture #{cls}>
      <source srcset="#{light}" media="(prefers-color-scheme: light)">
      <source srcset="#{dark}" media="(prefers-color-scheme: dark)">
      <img src="#{light}">
    </picture>
  |]
  where
    cls :: Text
    cls = if wide then "class=\"wide\"" else ""
