{-# LANGUAGE OverloadedStrings #-}

module Peits.Util where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson ((.=))
import qualified Data.Aeson as Ae
import qualified Data.HashMap.Strict as HM
import Data.List (foldl1')
import Data.Text (Text)
import qualified Data.Text.Lazy as Tx.L
import qualified Data.Text.Lazy.IO as Tx.L.IO
import qualified Development.Shake as S
import Peits.Types
import qualified Text.Mustache as Mu

getMatchingFiles :: S.FilePattern -> S.Action [FilePath]
getMatchingFiles = S.getDirectoryFiles "" . pure

json :: Ae.ToJSON v => Text -> v -> Ae.Value
json k v = Ae.object [k .= Ae.toJSON v]

mergeJSON :: [Ae.Value] -> Ae.Value
mergeJSON = foldl1' merge
  where
    merge (Ae.Object l) (Ae.Object r) = Ae.Object (HM.union l r)
    merge _ _ = error "Cannot merge non-object values"

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
