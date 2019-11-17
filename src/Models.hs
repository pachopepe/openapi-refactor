{-# LANGUAGE DuplicateRecordFields #-}

module Models
    ()
where

import           Data.Text                      ( Text )
import           Data.Int
import           Data.Map.Lazy                  ( Map(..) )
import qualified Data.Yaml                     as Y

    {-
data DataTypes
    = Integer Int64
    | Number Double
    | String [Char]
    deriving (Show, Eq)
-}

type Any = Y.Value


data Type
    = IntegerType
    | NumberType
    | StringType
    | Boolean
    deriving (Show, Eq)

data IntegerFormat
    = Int32
    | Int64
    deriving (Show, Eq)

data NumberFormat
    = Float
    | Double
    deriving (Show, Eq)

data StringFormat
    = NoStringFormat
    | Byte
    | Binary
    | Date
    | DateTime
    | Password
    deriving (Show, Eq)

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
    deriving (Show, Eq)

data InfoObject = InfoObject
    { title :: String
    , description :: Maybe String
    , termsOfService :: Maybe String
    , contact :: Maybe ContactObject
    , license :: Maybe LicenseObject
    , version :: String
    -- extensions :: Map String Any
    }
    deriving (Show, Eq)

data ContactObject = ContactObject
    { name :: Maybe String
    , url :: String
    , email :: String
    }
    -- extensions :: Map String Any
    deriving (Show, Eq)

data LicenseObject = LicenseObject
    { name :: String
    , url :: String
    }
    -- extensions :: Map String Any
    deriving (Show, Eq)

data ServerObject = ServerObject
    { url :: String
    , description :: Maybe String
    , variables :: Map String ServerVariableObject
    }
    deriving (Show, Eq)

data ServerVariableObject = ServerVariableObject
    { enum :: [String]
    , defaultValue :: String
    , description :: Maybe String
    }
    deriving (Show, Eq)

data ComponentsObject = ComponentsObject
    { schemas :: Map String (ReferenceWith SchemaObject)
    , responses :: Map String (ReferenceWith SchemaObject)
    , parameters :: Map String (ReferenceWith ParameterObject)
    , examples :: Map String (ReferenceWith ExampleObject)
    , headers :: Map String (ReferenceWith HeaderObject)
    , securitySchemes :: Map String (ReferenceWith SecuritySchemeObject)
    , links :: Map String (ReferenceWith LinkObject)
    , callbacks :: Map String (ReferenceWith CallbackObject)
    }
    deriving (Show, Eq)

data PathsObject = PathsObject String PathItemObject
    deriving (Show, Eq)

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
    deriving (Show, Eq)

data OperationObject = OperationObject
    { tags :: [String]
    , summary ::  Maybe String
    , externalDocs :: Maybe ExternalDocumentationObject
    , operationId :: Maybe String
    , parameters :: [ReferenceWith ParameterObject]
    , requestBody :: (ReferenceWith RequestObjectBody)
    , response :: ResponsesObject
    , callbacks :: (Map String (ReferenceWith CallbackObject))
    , deprecated :: Maybe Bool
    , security :: [SecurityRequirementObject]
    , servers :: [ServerObject]
    }
    deriving (Show, Eq)

data ExternalDocumentationObject = ExternalDocumentationObject
    { description :: Maybe String
    , url :: String
    }
    deriving (Show, Eq)

data InParameterObject
   = Query
   | Header
   | Path
   | Cookie
    deriving (Show, Eq)

data ParameterObject = ParameterObject
    { name :: String
    , inLocation :: String
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
    deriving (Show, Eq)

data RequestObjectBody = RequestObjectBody
    { description :: Maybe String
    , content :: (Map String MediaTypeObject)
    , required :: Maybe Bool
    }
    deriving (Show, Eq)

data MediaTypeObject = MediaTypeObject
    { schema :: (ReferenceWith SchemaObject)
    , example :: Any
    , examples :: (Map String (ReferenceWith ExampleObject))
    , encoding :: (Map String EncodingObject)
    }
    deriving (Show, Eq)

data EncodingObject = EncodingObject
    { contentType :: Maybe String
    , headers :: (Map String (ReferenceWith HeaderObject))
    , style :: Maybe String
    , explode :: Maybe Bool
    , allowReserved :: Maybe Bool
    }
    deriving (Show, Eq)


data ResponsesObject = ResponsesObject (Map String (ReferenceWith ResponseObject))
    deriving (Show, Eq)

data ResponseObject = ResponseObject
    { description :: String
    , headers :: Map String (ReferenceWith HeaderObject)
    , content :: Map String MediaTypeObject
    , links :: Map String (ReferenceWith LinkObject)
    }
    deriving (Show, Eq)

newtype CallbackObject
    = CallbackObject (Map String PathItemObject)
    deriving (Show, Eq)

data ExampleObject = ExampleObject
    { summary :: Maybe String
    , description :: Maybe String
    , value :: Any
    , externalValue :: Maybe String
    }
    deriving (Show, Eq)

type Expression = String

data LinkObject = LinkObject
    { operationRef :: Maybe String
    , operationId :: Maybe String
    , parameters :: Map String (Either Any Expression)
    , requestBody :: Maybe (Either Any Expression)
    , description :: Maybe String
    , server :: Maybe ServerObject
    }
    deriving (Show, Eq)

data HeaderObject = HeaderObject
    { description :: Maybe String
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
    deriving (Show, Eq)

data TagObject = TagObject
    { name :: String
    , description :: Maybe String
    , externalDocs :: Maybe ExternalDocumentationObject
    }
    deriving (Show, Eq)

newtype ReferenceObject
    = ReferenceObject { ref :: String }
    deriving (Show, Eq)

data NumberSchemaOptions t f
    = MultipleOf t
    | Minimum t
    | Maximum t
    | ExclusiveMinimum t
    | ExclusiveMaximum t
    | DefaultNumber t
    | NumberFormat f
    deriving (Show, Eq)

data StringSchemaOptions
    = MinLength Int
    | MaxLength Int
    | Enum [String]
    | DefaultString String
    | PatternValue String
    deriving (Show, Eq)

data ArraySchemaOptions
    = MinItems { maxitems :: Int}
    | MaxItems { minitems :: Int}
    | UniqueItems { uniqueitems :: Bool}
    | Items { items :: (ReferenceWith SchemaObject)}
    deriving (Show, Eq)

data ObjectSchemaOptions
    = MinProperties { minProperties :: Int}
    | MaxProperties { maxProperties :: Int}
    | Required { required :: [String]}
    deriving (Show, Eq)

data SchemaType
    = StringSchemaType [StringSchemaOptions]
    | ObjectSchemaType
        { properties :: Map String SchemaObject
        , options :: [ObjectSchemaOptions]
        , additionalProperties :: Either Bool (ReferenceWith SchemaObject)
        }
    | ArraySchemaType [ArraySchemaOptions]
    | BooleanSchemaType
    | NullSchemaType
    | IntegerSchemaType [NumberSchemaOptions Int IntegerFormat]
    | NumberSchemaType [NumberSchemaOptions Double NumberFormat]
    | AllOfSchemaType { allOf :: [SchemaObject], discriminator :: Maybe DiscriminatorObject}
    | OneOfSchemaType { oneOf :: [SchemaObject], discriminator :: Maybe DiscriminatorObject}
    | AnyOfSchemaType { anyOf :: [SchemaObject], discriminator :: Maybe DiscriminatorObject}
    | NotSchemaType SchemaObject
    deriving (Show, Eq)

data SchemaObject = SchemaObject
     -- Json Schema Object derived
    { schemaType :: SchemaType
    , title :: Maybe String
    , description :: Maybe String
    , nullable :: Maybe Bool
    , readOnly :: Maybe Bool
    , writeOnly :: Maybe Bool
    , xml :: XMLObject
    , externalDocs :: ExternalDocumentationObject
    , example :: Any
    , deprecated :: Bool
    }
    deriving (Show, Eq)

data DiscriminatorObject = DiscriminatorObject
    { propertyName :: String
    , mappint :: Map String String
    }
    deriving (Show, Eq)

data XMLOptions
    = NameOption { name :: String }
    | Namespace {namespace :: String }
    | Preffix { prefix :: String }
    | Attribure { attribute :: Bool }
    | Wrapped { wrapped :: Bool }
    deriving (Show, Eq)

data XMLObject = XMLObject [XMLOptions]
    deriving (Show, Eq)

data SecuritySchemeObject = SecuritySchemeObject
    { securitySchemeType :: String
    , description :: Maybe String
    , name :: String
    , securitySchemeIn :: String
    , scheme :: String
    , bearerFormat :: Maybe String
    , flows :: OAuthFlowsObject
    , openIdConnectUrl :: String
    }
    deriving (Show, Eq)

data OAuthFlowsObject = OAuthFlowsObject
    { authorizationUrl :: String
    , tokenUrl :: String
    , refreshUrl :: Maybe String
    , scopes :: Map String String
    }
    deriving (Show, Eq)

type SecurityRequirementObject = Map String [String]

type SpecificationExtensions = Map String Any
