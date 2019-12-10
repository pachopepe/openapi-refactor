module OpenApi3.FileUtils where

import           Data.Yaml
import           Data.Yaml.Pretty
import qualified Data.ByteString               as B
import qualified Data.Map.Lazy                 as M
import qualified Data.Set                      as S
import           OpenApi3.Models                ( OpenApiObject )
import           OpenApi3.Refactor              ( factorizeOpenApi, Dict(..) )
import           Control.Monad.State.Lazy

config = setConfCompare compare . setConfDropNull True $ defConfig

saveToDisk :: FilePath -> OpenApiObject -> IO ()
saveToDisk outputFile contents =
    B.writeFile outputFile . encodePretty config $ (contents :: OpenApiObject)

loadFromDisk :: FilePath -> IO (Either ParseException OpenApiObject)
loadFromDisk = decodeFileEither

refactor :: OpenApiObject -> OpenApiObject
refactor o = evalState (factorizeOpenApi o) (Dict (M.empty,S.empty) (M.empty,S.empty) (M.empty,S.empty))

process :: FilePath -> FilePath -> IO ()
process inputFile outputFile = do
    eContents <- loadFromDisk inputFile 
    case eContents of
      Left err -> error (show err)
      Right contents -> saveToDisk outputFile . refactor $ contents 

formatOnly :: FilePath -> FilePath -> IO ()
formatOnly inputFile outputFile = do
    eContents <- loadFromDisk inputFile 
    case eContents of
      Left err -> error (show err)
      Right contents -> saveToDisk outputFile contents 
