{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Peits.Util where

import Data.Aeson ((.=))
import qualified Data.Aeson as Ae
import qualified Data.HashMap.Strict as HM
import Data.List (foldl1')
import Data.Text (Text)
import qualified Data.Yaml as Yaml
import qualified Development.Shake as S

getMatchingFiles :: S.FilePattern -> S.Action [FilePath]
getMatchingFiles = S.getDirectoryFiles "" . pure

readYaml :: Ae.FromJSON a => FilePath -> IO a
readYaml fp = do
  Yaml.decodeFileEither fp >>= \case
    Left err -> fail (Yaml.prettyPrintParseException err)
    Right v -> pure v

json :: Ae.ToJSON v => Text -> v -> Ae.Value
json k v = Ae.object [k .= Ae.toJSON v]

mergeJSON :: [Ae.Value] -> Ae.Value
mergeJSON = foldl1' merge
  where
    merge (Ae.Object l) (Ae.Object r) = Ae.Object (HM.union l r)
    merge _ _ = error "Cannot merge non-object values"

newConstCache :: S.Action v -> S.Rules (S.Action v)
newConstCache action = fmap ($ ()) . S.newCache $ \() -> action
