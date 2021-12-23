{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Stork where
-- intentionally nongeneric, reuse with care

import           Toml (TomlCodec, (.=))
import qualified Toml
import qualified Data.Text as T
import           Data.Text (Text)
import qualified Data.Text.Encoding as E
import qualified Data.ByteString.Lazy as LBS
import qualified Hakyll as H

data File = File
    { fileTitle    :: Text
    , fileUrl      :: Text
    , filePath     :: Text
    , fileFileType :: Text
    }

fileCodec :: TomlCodec File
fileCodec = File
    <$> Toml.text "title"    .= fileTitle
    <*> Toml.text "url"      .= fileUrl
    <*> Toml.text "path"     .= filePath
    <*> Toml.text "filetype" .= fileFileType

data Output = Output
    { outputSaveNearestId :: Bool
    }

outputCodec :: TomlCodec Output
outputCodec = Output
    <$> Toml.bool "save_nearest_html_id" .= outputSaveNearestId

data Input = Input
    { inputFiles         :: [File]
    , inputUrlPrefix     :: Text
    , inputBaseDirectory :: Text
    , inputHtmlSelector  :: Text
    }

inputCodec :: TomlCodec Input
inputCodec = Input
    <$> Toml.list fileCodec "files" .= inputFiles
    <*> Toml.text "url_prefix"      .= inputUrlPrefix
    <*> Toml.text "base_directory"  .= inputBaseDirectory
    <*> Toml.text "html_selector"   .= inputHtmlSelector

data IndexerConfig = IndexerConfig
    { indexerInput  :: Input
    , indexerOutput :: Output
    }

indexerConfigCodec :: TomlCodec IndexerConfig
indexerConfigCodec = IndexerConfig
    <$> Toml.table inputCodec "input"   .= indexerInput
    <*> Toml.table outputCodec "output" .= indexerOutput

itemToFile :: H.Item String -> H.Compiler File
itemToFile i = do
    let identifier = H.itemIdentifier i
    title <- H.getMetadataField' identifier "title"
    routeMaybe <- H.getRoute identifier
    route <- case routeMaybe of
        Nothing -> fail $ "No route for " ++ (show identifier)
        Just x  -> return x

    return $ File
        { fileTitle = T.pack title
        , fileUrl = T.pack route
        , filePath = T.pack route
        , fileFileType = "HTML"
        }

encodeToLazyUtf8 :: Text -> LBS.ByteString
encodeToLazyUtf8 = LBS.fromChunks . return . E.encodeUtf8

render :: String -> [H.Item String] -> H.Compiler (H.Item LBS.ByteString)
render outPath is = do
    files <- sequenceA $ map itemToFile is
    let input = Input
            { inputFiles = files
            , inputUrlPrefix = "/"
            , inputBaseDirectory = T.pack $ outPath ++ "/"
            , inputHtmlSelector = "#content > article"
            }
    let output = Output
            { outputSaveNearestId = False
            }
    let toml = Toml.encode indexerConfigCodec (IndexerConfig input output)

    H.debugCompiler $ T.unpack toml

    storkDb <- H.unixFilterLBS "stork" storkArgs (encodeToLazyUtf8 toml)
    H.makeItem storkDb
        where storkArgs = ["build", "--input", "-", "--output", "-"]
