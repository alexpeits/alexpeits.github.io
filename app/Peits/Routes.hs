{-# LANGUAGE GADTs #-}

module Peits.Routes where

import qualified Data.HashMap.Strict as HM
import Development.Shake ((%>))
import qualified Development.Shake as S
import Development.Shake.FilePath ((-<.>), (</>))
import qualified Development.Shake.FilePath as SF
import Peits.Constants (buildDir)
import Peits.Util (getMatchingFiles, newConstCache)

postR :: Route InputAndOutput
postR = Mapping "posts/*.md" (-<.> "html")

postListR :: Route OnlyOutput
postListR = Fixed "index.html"

pageR :: Route InputAndOutput
pageR = Mapping "pages/*.md" (-<.> "html")

listPageR :: Route InputAndOutput
listPageR = Mapping "lists/*.yml" (-<.> "html")

draftR :: Route InputAndOutput
draftR = Mapping "drafts/*.md" (-<.> "html")

draftListR :: Route OnlyOutput
draftListR = Fixed "drafts.html"

cssR :: Route InputAndOutput
cssR = Mapping "static/css/*.css" SF.dropDirectory1

jsR :: Route InputAndOutput
jsR = Mapping "static/js/*.js" SF.dropDirectory1

imagesR :: Route InputAndOutput
imagesR = Mapping "static/images/*" SF.dropDirectory1

keybaseR :: Route InputAndOutput
keybaseR = Mapping "static/keybase.txt" SF.dropDirectory1

tagR :: Route OnlyOutput
tagR = Generated "tags/*.html"

tagListR :: Route OnlyOutput
tagListR = Fixed "tags.html"

atomFeedR :: Route OnlyOutput
atomFeedR = Fixed "atom.xml"

mermaidDiagramLightR :: Route OnlyOutput
mermaidDiagramLightR = Generated "assets/mermaid/light/*"

mermaidDiagramDarkR :: Route OnlyOutput
mermaidDiagramDarkR = Generated "assets/mermaid/dark/*"

type InputAndOutput = FilePath -> FilePath -> S.Action ()

type OnlyOutput = FilePath -> S.Action ()

data Route f where
  Mapping :: S.FilePattern -> (FilePath -> FilePath) -> Route InputAndOutput
  Generated :: S.FilePattern -> Route OnlyOutput
  Fixed :: FilePath -> Route OnlyOutput

buildRoute ::
  Route f ->
  f ->
  S.Rules ()
buildRoute (Mapping pat outf) f = do
  let mapOut x = buildDir </> outf x
  S.action $
    getMatchingFiles pat >>= S.need . fmap mapOut
  inputMap <- newConstCache $ do
    ifiles <- getMatchingFiles pat
    pure $ HM.fromList (zip (mapOut <$> ifiles) ifiles)
  mapOut pat %> \output -> do
    input <- (HM.! output) <$> inputMap
    f input output
buildRoute (Generated pat) f = do
  let outPat = buildDir </> pat
  outPat %> \output ->
    f output
buildRoute (Fixed outFile) f = do
  let outFile' = buildDir </> outFile
  S.want [outFile']
  outFile' %> \output ->
    f output
