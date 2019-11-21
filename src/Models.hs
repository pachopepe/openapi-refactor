{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Models
    ()
where

import           Data.Text                      ( Text )
import           Data.Int
import           Data.Map.Lazy                  ( Map(..) )
import           Data.Yaml
import           GHC.Generics
import           Control.Monad
import           Control.Applicative
import           Data.Maybe                     ( maybeToList )

    {-
data DataTypes
    = Integer Int64
    | Number Double
    | String [Char]
    deriving (Show, Eq)
-}

type Any = Value


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

instance FromJSON IntegerFormat where
    parseJSON = withText "IntegerFormat" parse
      where
        parse "int32" = pure Int32
        parse "int64" = pure Int64
        parse s =
            fail $ "Expected an IntegerFormat instead of " ++ show s ++ "'"



data NumberFormat
    = Float
    | Double
    deriving (Show, Eq)

instance FromJSON NumberFormat where
    parseJSON = withText "NumberFormat" parse
      where
        parse "float"  = pure Float
        parse "double" = pure Double
        parse s =
            fail $ "Expected a NumberFormat instead of '" ++ show s ++ "'"

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
    , requestBody :: ReferenceWith RequestObjectBody
    , response :: ResponsesObject
    , callbacks :: Map String (ReferenceWith CallbackObject)
    , deprecated :: Maybe Bool
    , security :: [SecurityRequirementObject]
    , servers :: [ServerObject]
    }
    deriving (Show, Eq)

data ExternalDocumentationObject = ExternalDocumentationObject
    { description :: Maybe String
    , url :: String
    }
    deriving (Show, Eq, Generic)

instance FromJSON ExternalDocumentationObject

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
    deriving (Show, Eq, Generic)

instance FromJSON ReferenceObject

data NumberSchemaOptions t f
    = MultipleOf { multipleOf :: t }
    | Minimum { minimum :: t }
    | Maximum { maximum :: t }
    | ExclusiveMinimum { exclusiveMinimum :: t }
    | ExclusiveMaximum { exclusiveMaximum :: t }
    | DefaultNumber { defaultNumber :: t }
    | NumberFormat { format :: f }
    deriving (Show, Eq)


data StringSchemaOptions
    = MinLength { minLength :: Int }
    | MaxLength { maxLength :: Int }
    | Enum { enum :: [String] }
    | DefaultString { defaultString :: String }
    | PatternValue { patternValue :: String }
    deriving (Show, Eq)

data ArraySchemaOptions
    = MinItems { maxitems :: Int}
    | MaxItems { minitems :: Int}
    | UniqueItems { uniqueitems :: Bool}
    | Items { items :: ReferenceWith SchemaObject}
    deriving (Show, Eq, Generic)

instance FromJSON ArraySchemaOptions

data ObjectSchemaOptions
    = MinProperties { minProperties :: Int}
    | MaxProperties { maxProperties :: Int}
    | Required { required :: [String]}
    deriving (Show, Eq, Generic)

instance FromJSON ObjectSchemaOptions

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
    | NotSchemaType { not:: SchemaObject}
    deriving (Show, Eq)

parseNumberSchemaType
    :: (FromJSON t, FromJSON f)
    => String
    -> ([NumberSchemaOptions t f] -> SchemaType)
    -> Value
    -> Parser SchemaType
parseNumberSchemaType str ctr = withObject str $ \o -> do
    multipleOfL       <- map MultipleOf <$> o .::? "multipleOf"
    minimumL          <- map Minimum <$> o .::? "minimum"
    maximumL          <- map Maximum <$> o .::? "maximum"
    exclusiveMinimumL <- map ExclusiveMinimum <$> o .::? "exclusiveMinimum"
    exclusiveMaximumL <- map ExclusiveMaximum <$> o .::? "exclusiveMaximum"
    defaultNumberL    <- map DefaultNumber <$> o .::? "default"
    formatL           <- map NumberFormat <$> o .::? "format"
    return
        . ctr
        . concat
        $ [ multipleOfL
          , minimumL
          , maximumL
          , exclusiveMinimumL
          , exclusiveMaximumL
          , defaultNumberL
          , formatL
          ]

parseStringSchemaType :: Value -> Parser SchemaType
parseStringSchemaType = withObject "String" $ \o -> do
    minLengthL     <- map MinLength <$> o .::? "minLength"
    maxLengthL     <- map MaxLength <$> o .::? "maxLength"
    enumL          <- map Enum <$> o .::? "enum"
    defaultStringL <- map DefaultString <$> o .::? "defaultString"
    patternValueL  <- map PatternValue <$> o .::? "patternValue"
    return
        . StringSchemaType
        . concat
        $ [minLengthL, maxLengthL, enumL, defaultStringL, patternValueL]

parseSchemaType :: String -> Value -> Parser SchemaType
parseSchemaType str@"integer" = parseNumberSchemaType str IntegerSchemaType
parseSchemaType str@"number"  = parseNumberSchemaType str NumberSchemaType
parseSchemaType "string"      = parseStringSchemaType

data SchemaObject = SchemaObject
     -- Json Schema Object derived
    { schemaType :: (String, SchemaType)
    , title :: Maybe String
    , description :: Maybe String
    , discriminator :: Maybe DiscriminatorObject
    , nullable :: Maybe Bool
    , readOnly :: Maybe Bool
    , writeOnly :: Maybe Bool
    , xml :: Maybe XMLObject
    , externalDocs :: Maybe ExternalDocumentationObject
    , example :: Maybe Any
    , deprecated :: Maybe Bool
    }
    deriving (Show, Eq)


instance FromJSON SchemaObject where
    parseJSON val = withObject
        "SchemaObject"
        (\o -> do
            title         <- o .:? "title"
            description   <- o .:? "description"
            theType       <- o .: "type"
            schemaType    <- (theType, ) <$> parseSchemaType theType val
            nullable      <- o .:? "nullable"
            discriminator <- o .:? "discriminator"
            readOnly      <- o .:? "readOnly"
            writeOnly     <- o .:? "writeOnly"
            xml           <- o .:? "xml"
            externalDocs  <- o .:? "externalDocs"
            example       <- o .:? "example"
            deprecated    <- o .:? "deprecated"
            return $ SchemaObject { .. }
        )
        val


data DiscriminatorObject = DiscriminatorObject
    { propertyName :: String
    , mappint :: Map String String
    }
    deriving (Show, Eq, Generic)

instance FromJSON DiscriminatorObject

instance ToJSON DiscriminatorObject

data XMLOptions
    = NameOption { name :: String }
    | Namespace {namespace :: String }
    | Prefix { prefix :: String }
    | Attribute { attribute :: Bool }
    | Wrapped { wrapped :: Bool }
    deriving (Show, Eq, Generic)

instance FromJSON XMLOptions


pairXMLOptions (NameOption    name     ) = ("name", toJSON name)
pairXMLOptions (Namespace     namespace) = ("namespace", toJSON namespace)
pairXMLOptions (Models.Prefix prefix   ) = ("prefix", toJSON prefix)
pairXMLOptions (Attribute     attribute) = ("attribute", toJSON attribute)
pairXMLOptions (Wrapped       wrapped  ) = ("wrapped", toJSON wrapped)

newtype XMLObject = XMLObject [XMLOptions]
    deriving (Show, Eq)

(.::?) :: FromJSON a => Object -> Text -> Parser [a]
o .::? field = maybeToList <$> o .:? field

instance FromJSON XMLObject where
    parseJSON = withObject
        "XMLObject"
        (\o -> do
            nameL      <- map NameOption <$> o .::? "name"
            namespaceL <- map Namespace <$> o .::? "namespace"
            prefixL    <- map Models.Prefix <$> o .::? "prefix"
            attributeL <- map Attribute <$> o .::? "attribute"
            wrappedL   <- map Wrapped <$> o .::? "wrapped"
            return
                . XMLObject
                . concat
                $ [nameL, namespaceL, prefixL, attributeL, wrappedL]
        )

instance ToJSON XMLObject where
    toJSON (XMLObject options) = object $ map pairXMLOptions options

data SecuritySchemeObject = SecuritySchemeObject
    { securitySchemeType :: SecuritySchemeObjectType
    , description :: Maybe String
    , name :: Maybe String
    , securitySchemeIn :: Maybe String
    , scheme :: Maybe String
    , bearerFormat :: Maybe String
    , flows :: Maybe OAuthFlowsObject
    , openIdConnectUrl :: Maybe String
    }
    deriving (Show, Eq, Generic)

requiredFor True  o field = o .: field
requiredFor False o field = o .:? field

instance FromJSON SecuritySchemeObject where
    parseJSON = withObject "SecuritySchemeObject" $ \o -> do
        securitySchemeType <- o .: "type"
        description        <- o .:? "description"
        name               <- requiredFor (isApiKey securitySchemeType) o "name"
        securitySchemeIn   <- requiredFor (isApiKey securitySchemeType) o "in"
        scheme             <- requiredFor (isHttp securitySchemeType) o "scheme"
        bearerFormat <- requiredFor (isHttp securitySchemeType) o "bearerFormat"
        flows <- requiredFor (isOAuth2 securitySchemeType) o "flows"
        openIdConnectUrl   <- requiredFor (isOpenIdConnect securitySchemeType)
                                          o
                                          "openIdConnectUrl"
        return $ SecuritySchemeObject { .. }
      where
        isApiKey        = (== ApiKey)
        isHttp          = (== Http)
        isOAuth2        = (== OAuth2)
        isOpenIdConnect = (== OpenIdConnect)

instance ToJSON SecuritySchemeObject where
    toJSON SecuritySchemeObject {..} =
        object $ ["type" .= securitySchemeType] ++ concat
            [ "description" .=? description
            , "name" .=? name
            , "in" .=? securitySchemeIn
            , "scheme" .=? scheme
            , "bearerFormat" .=? bearerFormat
            , "flows" .=? flows
            , "openIdConnectUrl" .=? openIdConnectUrl
            ]

data SecuritySchemeObjectType
    = ApiKey
    | Http
    | OAuth2
    | OpenIdConnect
    deriving (Show, Eq)

instance FromJSON SecuritySchemeObjectType where
    parseJSON = withText "SecuritySchemeObjectType" parse
      where
        parse "apiKey"        = pure ApiKey
        parse "http"          = pure Http
        parse "oauth2"        = pure OAuth2
        parse "openIdConnect" = pure OpenIdConnect
        parse s =
            fail
                $  "Expected an SecuritySchemeObjectType instead of "
                ++ show s
                ++ "'"

instance ToJSON SecuritySchemeObjectType where
    toJSON ApiKey        = toJSON ("apiKey" :: String)
    toJSON Http          = toJSON ("http" :: String)
    toJSON OAuth2        = toJSON ("oauth2" :: String)
    toJSON OpenIdConnect = toJSON ("openIdConnect" :: String)

data OAuthFlowsObject = OAuthFlowsObject
    { implicit :: Maybe OAuthFlowObject
    , password :: Maybe OAuthFlowObject
    , clientCredentianls :: Maybe OAuthFlowObject
    , authorizationCode :: Maybe OAuthFlowObject
    }
    deriving (Show, Eq, Generic)

instance FromJSON OAuthFlowsObject

instance ToJSON OAuthFlowsObject where
    toJSON OAuthFlowsObject {..} = object $ concat
        [ "implicit" .=? implicit
        , "password" .=? password
        , "clientCredentianls" .=? clientCredentianls
        , "authorizationCode" .=? authorizationCode
        ]

data OAuthFlowObject = OAuthFlowObject
    { authorizationUrl :: Maybe String
    , tokenUrl :: Maybe String
    , refreshUrl :: Maybe String
    , scopes :: Map String String
    }
    deriving (Show, Eq, Generic)

instance FromJSON OAuthFlowObject

infixr 8 .=?

field .=? elem = maybe [] ((: []) . (field .=)) elem

instance ToJSON OAuthFlowObject where
    toJSON OAuthFlowObject {..} = object $ concat
        [ "authorizationUrl" .=? authorizationUrl
        , "tokenUrl" .=? tokenUrl
        , "refreshUrl" .=? refreshUrl
        , ["scopes" .= scopes]
        ]

type SecurityRequirementObject = Map String [String]

type SpecificationExtensions = Map String Any


