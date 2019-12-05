module OpenApi3.FileUtils where

import           OpenApi3.Models                ( OpenApiObject )
import           Data.Yaml
import           Data.Yaml.Pretty
import qualified Data.ByteString               as B

config = setConfCompare compare . setConfDropNull True $ defConfig

saveToDisk outputFile contents =
    B.writeFile outputFile . encodePretty config $ (contents :: OpenApiObject)

loadFromDisk :: FilePath -> IO (Either ParseException OpenApiObject)
loadFromDisk = decodeFileEither

