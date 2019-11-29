
import Test.Hspec
import Data.Either
import Data.Yaml
import OpenApi3.Models



main :: IO ()
main = hspec $ do
    describe "Decode partial OpenApi3 yaml files" $ do
        it "Correct decode Info Object" $ do
           info <- decodeFileEither "Yaml/infoObject.yaml" :: IO (Either ParseException InfoObject) 
           info `shouldSatisfy` isRight 

        it "Correct decode Contact Object" $ do
           info <- decodeFileEither "Yaml/contactObject.yaml" :: IO (Either ParseException ContactObject) 
           info `shouldSatisfy` isRight 

        it "Correct decode Schema Object" $ do
           info <- decodeFileEither "Yaml/schemaObject.yaml" :: IO (Either ParseException SchemaObject) 
           info `shouldSatisfy` isRight 

        it "Correct decode Schema Object All Of" $ do
           info <- decodeFileEither "Yaml/schemaObjectAllOf.yaml" :: IO (Either ParseException SchemaObject) 
           info `shouldSatisfy` isRight 

        it "Correct decode Schema Object One Of" $ do
           info <- decodeFileEither "Yaml/schemaObjectOneOf.yaml" :: IO (Either ParseException SchemaObject) 
           info `shouldSatisfy` isRight 

        it "Correct decode Schema Object One Of with discriminator" $ do
           info <- decodeFileEither "Yaml/schemaObjectOneOfWithDiscriminator.yaml" :: IO (Either ParseException SchemaObject) 
           info `shouldSatisfy` isRight 

        it "Correct decode Security Schema Object" $ do
           info <- decodeFileEither "Yaml/securitySchemeObject.yaml" :: IO (Either ParseException SecuritySchemeObject) 
           info `shouldSatisfy` isRight 

        it "Correct decode Tag Object" $ do
           info <- decodeFileEither "Yaml/tagObject.yaml" :: IO (Either ParseException TagObject) 
           info `shouldSatisfy` isRight 

        it "Correct decode Parameter Object 1" $ do
           info <- decodeFileEither "Yaml/parameterObject1.yaml" :: IO (Either ParseException ParameterObject) 
           info `shouldSatisfy` isRight 

        it "Correct decode Header Object" $ do
           info <- decodeFileEither "Yaml/headerObject.yaml" :: IO (Either ParseException HeaderObject) 
           info `shouldSatisfy` isRight 

        it "Correct decode parameterObject 1" $ do
           info <- decodeFileEither "Yaml/parameterObject1.yaml" :: IO (Either ParseException ParameterObject) 
           info `shouldSatisfy` isRight 

        it "Correct decode parameterObject 2" $ do
           info <- decodeFileEither "Yaml/parameterObject2.yaml" :: IO (Either ParseException ParameterObject) 
           info `shouldSatisfy` isRight 

        it "Correct decode parameterObject 3" $ do
           info <- decodeFileEither "Yaml/parameterObject3.yaml" :: IO (Either ParseException ParameterObject) 
           info `shouldSatisfy` isRight 

        it "Correct decode parameterObject 4" $ do
           info <- decodeFileEither "Yaml/parameterObject4.yaml" :: IO (Either ParseException ParameterObject) 
           info `shouldSatisfy` isRight 

        it "Correct decode request body" $ do
           info <- decodeFileEither "Yaml/requestBody.yaml" :: IO (Either ParseException RequestObjectBody) 
           info `shouldSatisfy` isRight 

        it "Correct decode request body 1" $ do
           info <- decodeFileEither "Yaml/requestBody1.yaml" :: IO (Either ParseException RequestObjectBody) 
           info `shouldSatisfy` isRight 

        it "Correct decode responses" $ do
           info <- decodeFileEither "Yaml/responsesObject.yaml" :: IO (Either ParseException ResponsesObject) 
           info `shouldSatisfy` isRight 

        it "Correct decode response object" $ do
           info <- decodeFileEither "Yaml/responseObject.yaml" :: IO (Either ParseException ResponseObject) 
           info `shouldSatisfy` isRight 

        it "Correct decode response object 1" $ do
           info <- decodeFileEither "Yaml/responseObject1.yaml" :: IO (Either ParseException ResponseObject) 
           info `shouldSatisfy` isRight 

        it "Correct decode response object 2" $ do
           info <- decodeFileEither "Yaml/responseObject2.yaml" :: IO (Either ParseException ResponseObject) 
           info `shouldSatisfy` isRight 

        it "Correct decode response object 3" $ do
           info <- decodeFileEither "Yaml/responseObject3.yaml" :: IO (Either ParseException ResponseObject) 
           info `shouldSatisfy` isRight 

        it "Correct decode media type object" $ do
           info <- decodeFileEither "Yaml/mediaTypeObject.yaml" :: IO (Either ParseException MediaTypeObject) 
           info `shouldSatisfy` isRight 

        it "Correct decode media type object 1" $ do
           info <- decodeFileEither "Yaml/mediaTypeObject1.yaml" :: IO (Either ParseException MediaTypeObject) 
           info `shouldSatisfy` isRight 

        it "Correct decode media type object 2" $ do
           info <- decodeFileEither "Yaml/mediaTypeObject2.yaml" :: IO (Either ParseException MediaTypeObject) 
           info `shouldSatisfy` isRight 

        it "Correct decode media link object" $ do
           info <- decodeFileEither "Yaml/linkObject.yaml" :: IO (Either ParseException LinkObject) 
           info `shouldSatisfy` isRight 

        it "Correct decode media link object 1" $ do
           info <- decodeFileEither "Yaml/linkObject1.yaml" :: IO (Either ParseException LinkObject) 
           info `shouldSatisfy` isRight 

        it "Correct decode media link object 2" $ do
           info <- decodeFileEither "Yaml/linkObject2.yaml" :: IO (Either ParseException LinkObject) 
           info `shouldSatisfy` isRight 

        it "Correct decode example object" $ do
           info <- decodeFileEither "Yaml/exampleObject.yaml" :: IO (Either ParseException ExampleObject) 
           info `shouldSatisfy` isRight 

        it "Correct decode example object 1" $ do
           info <- decodeFileEither "Yaml/exampleObject1.yaml" :: IO (Either ParseException ExampleObject) 
           info `shouldSatisfy` isRight 


