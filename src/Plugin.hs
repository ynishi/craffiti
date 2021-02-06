{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}

module Plugin
  ( PrepData(..)
  , extWith
  , pdr2pd
  , PrepDataRaw(..)
  ) where

import Control.Applicative
import Data.ByteString (ByteString)
import Data.ByteString.UTF8 as BSU
import qualified Data.Yaml as Y
import RIO
import RIO.List as RL
import Text.RawString.QQ

data PrepData
  = PrepData { pdInits :: PrepData
             , pdInstalls :: PrepData
             , pdFiles :: PrepData }
  | PDInit { pdiVars :: PrepData
           , pdiLines :: [PrepData] }
  | PDInstall { pdisVars :: PrepData
              , pdisLines :: [PrepData] }
  | PDFiles { pdfsVars :: PrepData
            , pdfsFiles :: [PrepData] }
  | PDLine [PrepData]
  | PDFile { pdfName :: PrepData
           , pdfPath :: PrepData
           , pdfContent :: PrepData }
  | PDVars [(String, PrepData)]
  | PDVar String
  | PDConst String
  | PDEmpty
  deriving (Show)

toTuple (PrepData init install files) = (init, install, files)

toTupleExt pd = (ext init, ext install, ext files)
  where
    (init, install, files) = toTuple pd

extWith :: [(String, String)] -> PrepData -> [String]
extWith vars p@PDInit {} = ext p {pdiVars = toVars vars}
extWith vars p@PDInstall {} = ext p {pdisVars = toVars vars}
extWith vars p@PDFiles {} = ext p {pdfsVars = toVars vars}

ext :: PrepData -> [String]
ext PDEmpty = []
ext (PDConst s) = [s]
ext (PDInit (PDVars vars) lines) = intercalate ["&&"] $ map (extVars vars) lines
ext (PDInstall (PDVars vars) lines) =
  intercalate ["&&"] $ map (extVars vars) lines
ext (PDFiles (PDVars vars) files) = concatMap (extVars vars) files

extVars :: [(String, PrepData)] -> PrepData -> [String]
extVars vars (PDVar s) =
  ext . snd . fromMaybe ("", PDEmpty) . RL.headMaybe . filter ((== s) . fst) $
  vars
extVars vars (PDLine xs) = concatMap (extVars vars) xs
extVars vars PDFile {pdfName, pdfPath, pdfContent} =
  (\(n:p:c:_) -> [p ++ "/" ++ n, c]) $
  concatMap (extVars vars) [pdfName, pdfPath, pdfContent]
extVars _ pd = ext pd

data PrepDataRaw = PrepDataRaw
  { init :: Maybe String
  , inits :: Maybe [String]
  , install :: Maybe String
  , installs :: Maybe [String]
  , files :: Maybe [FileRaw]
  } deriving (Show)

data FileRaw = FileRaw
  { name :: String
  , path :: Maybe String
  , content :: Maybe String
  } deriving (Show)

instance Y.FromJSON PrepDataRaw where
  parseJSON (Y.Object v) =
    PrepDataRaw <$> v Y..:? "init" <*> v Y..:? "inits" <*> v Y..:? "install" <*>
    v Y..:? "installs" <*>
    v Y..:? "files"
  parseJSON _ = fail "Expected Object for PrepDataRaw value"

instance Y.FromJSON FileRaw where
  parseJSON (Y.Object v) =
    FileRaw <$> v Y..: "name" <*> v Y..:? "path" <*> v Y..:? "content"
  parseJSON _ = fail "Expected Object for FileRaw value"

pdr2pd :: [(String, String)] -> PrepDataRaw -> PrepData
pdr2pd varData pdr =
  PrepData
    (PDInit vars (lines init Plugin.inits))
    (PDInstall vars (lines install installs))
    (PDFiles vars fs)
  where
    parse s@(x:_) =
      if x == '{' && '}' == fromMaybe ' ' (headMaybe $ reverse s)
        then PDVar s
        else PDConst s
    lines f fs =
      map (PDLine . map parse . words) $ fpdr ++ fromMaybe [] (fs pdr)
      where
        fpdr =
          case f pdr of
            Just x -> [x]
            Nothing -> []
    filesList = fromMaybe [] $ files pdr
    fs =
      map
        (\file ->
           PDFile
             (parse . name $ file)
             (parse . fromMaybe "." . path $ file)
             (parse . fromMaybe "" . content $ file))
        filesList
    vars = toVars varData

toVars :: [(String, String)] -> PrepData
toVars xs =
  PDVars $
  map
    (\(name, value) -> ("{" ++ (unwords . words $ name) ++ "}", PDConst value))
    xs
