{-# LANGUAGE OverloadedStrings #-}

module Peits.Template where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Ae
import Data.Text (Text)
import qualified Data.Text.Lazy as Tx.L
import qualified Data.Text.Lazy.IO as Tx.L.IO
import qualified Development.Shake as S
import Peits.Config (Config)
import Peits.Util (json, mergeJSON)
import qualified Text.Mustache as Mu

renderTemplate ::
  -- | global config
  Config ->
  -- | all compiled templates
  Mu.Template ->
  -- | list of templates to use
  [Mu.PName] ->
  -- | content to interpolate
  Maybe Text ->
  -- | context values
  [Ae.Value] ->
  -- | output file
  FilePath ->
  S.Action ()
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
