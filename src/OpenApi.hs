module OpenApi
    ( refactor
    )
where

import           OpenApi3.Models                ( OpenApiObject )
import           Data.Yaml
import           Data.Yaml.Pretty
import           System.Environment             ( getArgs )
import qualified Data.ByteString               as B

config = setConfCompare compare . setConfDropNull True $ defConfig

saveToDisk outputFile contents =
    B.writeFile outputFile . encodePretty config $ (contents :: OpenApiObject)

factorizeEquals :: () -> OpenApiObject -> OpenApiObject
factorizeEquals = undefined



refactor :: IO ()
refactor = do
    [inputFile, outputFile] <- getArgs
    eContent                <- decodeFileEither inputFile
    case eContent of
        Left  err      -> error $ show err
        Right contents -> do
            saveToDisk outputFile contents
            return ()
             {-
             encodeFileWith
                                defaultEncodeOptions
                                outputFile
                                (contents :: OpenApiObject)
-}
