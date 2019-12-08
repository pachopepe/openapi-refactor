module OpenApi3.FileUtils where

import           Data.Yaml
import           Data.Yaml.Pretty
import qualified Data.ByteString               as B
import qualified Data.Map.Lazy                 as M
import           OpenApi3.Models                ( OpenApiObject )
import           OpenApi3.Refactor
import           Control.Monad.State.Lazy

config = setConfCompare compare . setConfDropNull True $ defConfig

saveToDisk outputFile contents =
    B.writeFile outputFile . encodePretty config $ (contents :: OpenApiObject)

loadFromDisk :: FilePath -> IO (Either ParseException OpenApiObject)
loadFromDisk = decodeFileEither

refactor :: OpenApiObject -> OpenApiObject
refactor o = evalState (factorizeOpenApi o) (Dict M.empty M.empty M.empty)
