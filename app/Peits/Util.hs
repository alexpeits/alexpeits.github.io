{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Peits.Util where

import qualified Crypto.Hash as Crypto
import Data.Aeson ((.=))
import qualified Data.Aeson as Ae
import qualified Data.Aeson.KeyMap as KM
import Data.ByteString (ByteString)
import Data.List (foldl1')
import Data.String.Interpolate (i)
import Data.Text (Text)
import qualified Data.Text as Tx
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Yaml as Yaml
import qualified Development.Shake as S

getMatchingFiles :: S.FilePattern -> S.Action [FilePath]
getMatchingFiles = S.getDirectoryFiles "" . pure

needNodeExecutable :: FilePath -> S.Action ()
needNodeExecutable bin = S.need [binPath]
  where
    binPath = [i|node_modules/.bin/#{bin}|]

readYaml :: Ae.FromJSON a => FilePath -> IO a
readYaml fp = do
  Yaml.decodeFileEither fp >>= \case
    Left err -> fail (Yaml.prettyPrintParseException err)
    Right v -> pure v

hashText :: Text -> Text
hashText = Tx.pack . show . hash . encodeUtf8
  where
    hash :: ByteString -> Crypto.Digest Crypto.SHA1
    hash = Crypto.hash

json :: Ae.ToJSON v => Ae.Key -> v -> Ae.Value
json k v = Ae.object [k .= Ae.toJSON v]

mergeJSON :: [Ae.Value] -> Ae.Value
mergeJSON = foldl1' merge
  where
    merge (Ae.Object l) (Ae.Object r) = Ae.Object (KM.union l r)
    merge _ _ = error "Cannot merge non-object values"

newConstCache :: S.Action v -> S.Rules (S.Action v)
newConstCache action = fmap ($ ()) . S.newCache $ \() -> action
