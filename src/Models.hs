{-# LANGUAGE DuplicateRecordFields #-}

module Models
    ()
where

import Data.Text ()
import Data.Int
import Data.Map ( Map )

    {-
data DataTypes
    = Integer Int64
    | Number Double
    | String [Char]
    deriving (Show, Eq, Ord)
-}

data Type
    = IntegerType
    | NumberType
    | StringType
    | Boolean

data IntegerFormat
    = Int32
    | Int64
    
data NumberFormat
    = Float
    | Double

data StringFormat
    = NoStringFormat
    | Byte
    | Binary
    | Date
    | DateTime
    | Password

data OpenapiElement
    = Openapi String
     
     
    deriving (Show, Eq, Ord)

type ReferenceWith a = Either ReferenceObject a

data OpenApiObject = OpenApiObject
    { openapi :: String
    , info :: InfoObject
    , servers :: [ServerObject]
    , paths :: PathsObject
    , components :: Maybe ComponentsObject
    , security :: [SecurityRequirementObject]
    , tags :: [TagObject]
    , externalDocs :: Maybe ExternalDocumentationObject  
    }
    deriving (Show, Eq, Ord)

data InfoObject = InfoObject 
    { title :: String
    , description :: Maybe String
    , termsOfService :: Maybe String
    , contact :: Maybe ContactObject
    , license :: Maybe LicenseObject
    , version :: String
    -- extensions :: Map String Any
    }
    deriving (Show, Eq, Ord)

data ContactObject = ContactObject
    { name :: Maybe String
    , url :: String
    , email :: String
    }
    -- extensions :: Map String Any
    deriving (Show, Eq, Ord)

data LicenseObject = LicenseObject
    { name :: String
    , url :: String
    }
    -- extensions :: Map String Any
    deriving (Show, Eq, Ord)

data ServerObject = ServerObject
    { url :: String
    , description :: Maybe String
    , variables (Map String ServerVariableObject)
    }
    deriving (Show, Eq, Ord)

data ServerVariableObject = ServerVariableObject
    { enum :: [String]
    , default :: String
    , description :: Maybe String
    }
    deriving (Show, Eq, Ord)

data ComponentsObject = ComponentsObject
    { schemas :: (Map String (ReferenceWith SchemaObject))
    , responses :: (Map String (ReferenceWith SchemaObject))
    , parameters :: (Map String (ReferenceWith ParameterObject))
    , examples :: (Map String (ReferenceWith ExampleObject))
    , headers :: (Map String (ReferenceWith HeaderObject))
    , securitySchemes :: (Map String (ReferenceWith SecuritySchemeObject))
    , links :: (Map String (ReferenceWith LinkObject))
    , callbacks :: (Map String (ReferenceWith CallbackObject))
    }
    deriving (Show, Eq, Ord)

newtype PathsObject = PathsObject String PathItemObject

data PathItemObject = PathItemObject
    { ref :: Maybe String
    , summary :: Maybe String
    , description :: Maybe String
    , get :: Maybe OperationObject
    , put :: Maybe OperationObject
    , post :: Maybe OperationObject
    , delete :: Maybe OperationObject
    , options :: Maybe OperationObject
    , head :: Maybe OperationObject
    , patch :: Maybe OperationObject
    , trace :: Maybe OperationObject
    , servers :: [ServerObject]
    , parameters :: [ReferenceWith ParameterObject]
    }
    deriving (Show, Eq, Ord)

data OperationObject = OperationObject
    { tags :: [String]
    , summary ::  Maybe String
    , externalDocs :: Maybe ExternalDocumentationObject
    , operationId :: String
    , parameters :: [ReferenceWith ParameterObject]
    , RequestBody (ReferenceWith RequestObjectBody)
    , response ResponsesObject
    , callbacks (Map String (ReferenceWith CallbackObject))
    , deprecated Bool
    , security [SecurityRequirementObject]
    , servers [ServerObject]
    }
    deriving (Show, Eq, Ord)

data ExternalDocumentationObject = ExternalDocumentationObject
    { description :: Maybe String
    , url :: String
    }
    deriving (Show, Eq, Ord)

data InParameterObject
   = Query
   | Header
   | Path
   | Cookie
    deriving (Show, Eq, Ord)

data ParameterObject = ParameterObject
    { name :: String
    , in :: String
    , description :: Maybe String
    , required :: Maybe Bool
    , deprecated :: Maybe Bool
    , allowEmptyValue :: Bool
    , style :: String
    , explode :: Bool
    , allowReserved :: Bool

    , schema :: (ReferenceWith SchemaObject)
    , example :: Any
    , examples :: (Map String (ReferenceWith ExampleObject))
    , content :: (Map String MediaTypeObject)
    }
    deriving (Show, Eq, Ord)

data RequestObjectBody = RequestObjectBody [OpenapiElement]
    { description :: Maybe String
    , content :: (Map String MediaTypeObject)
    , required :: Maybe Bool
    }
    deriving (Show, Eq, Ord)

data MediaTypeObject = MediaTypeObject [OpenapiElement]
    { schema :: (ReferenceWith SchemaObject)
    , example :: Any
    , examples :: (Map String (ReferenceWith ExampleObject))
    , encoding :: (Map String EncodingObject)
    }
    deriving (Show, Eq, Ord)

data EncodingObject = EncodingObject
    { contentType :: Maybe String
    , headers :: (Map String (ReferenceWith HeaderObject))
    , style :: Maybe String
    , explode :: Maybe Bool
    , allowReserved :: Maybe Bool
    }
    deriving (Show, Eq, Ord)


data ResponsesObject = ResponsesObject (Map String (ReferenceWith ResponseObject))
    deriving (Show, Eq, Ord)

data ResponseObject = ResponseObject
    { description :: String
    , headers :: Map String (ReferenceWith HeaderObject)
    , content :: Map String MediaTypeObject
    , links :: Map String (ReferenceWith LinkObject) 
    }

data CallbackObject = CallbackObject (Map String PathItemObject)


