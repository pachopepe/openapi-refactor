{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module OpenApi3.Models where

import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Int
import           Data.Map.Lazy                  ( Map(..) )
import qualified Data.Map.Lazy                 as M
import qualified Data.HashMap.Strict           as MS
import           Data.Yaml
import           GHC.Generics
import           Control.Monad
import           Control.Applicative
import           Data.Maybe                     ( maybeToList
                                                , fromMaybe
                                                )
import qualified Debug.Trace                   as TR
import           Data.Hashable

    {-
data DataTypes
    = Integer Int64
    | Number Double
    | String [Char]
    deriving (Show, Eq)
-}

instance (Hashable k, Hashable v) => Hashable (M.Map k v) where
    hashWithSalt salt m = hashWithSalt salt (M.toList m)

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
    deriving (Show, Eq, Generic)

instance FromJSON IntegerFormat where
    parseJSON = withText "IntegerFormat" parse
      where
        parse "int32" = pure Int32
        parse "int64" = pure Int64
        parse s =
            fail $ "Expected an IntegerFormat instead of " ++ show s ++ "'"

instance ToJSON IntegerFormat where
    toJSON Int32 = String "int32"
    toJSON Int64 = String "int64"

instance Hashable IntegerFormat

data NumberFormat
    = Float
    | Double
    deriving (Show, Eq, Generic)

instance FromJSON NumberFormat where
    parseJSON = withText "NumberFormat" parse
      where
        parse "float"  = pure Float
        parse "double" = pure Double
        parse s =
            fail $ "Expected a NumberFormat instead of '" ++ show s ++ "'"

instance ToJSON NumberFormat where
    toJSON Float  = String "float"
    toJSON Double = String "double"

instance Hashable NumberFormat

data StringFormat
    = NoStringFormat
    | Byte
    | Binary
    | Date
    | DateTime
    | Password
    deriving (Show, Eq, Generic)

data ReferenceWith a
    = Reference ReferenceObject
    | Inline a
    deriving (Show, Eq, Generic)

instance FromJSON a => FromJSON (ReferenceWith a) where
    parseJSON value =
        (Reference <$> parseJSON value) <|> (Inline <$> parseJSON value)

instance ToJSON a => ToJSON (ReferenceWith a) where
    toJSON (Reference ref   ) = toJSON ref
    toJSON (Inline    inline) = toJSON inline

instance Hashable a => Hashable (ReferenceWith a)

data OpenApiObject = OpenApiObject
    { openapi :: String
    , info :: InfoObject
    , servers :: Maybe [ServerObject]
    , paths :: PathsObject
    , components :: Maybe ComponentsObject
    , security :: Maybe [SecurityRequirementObject]
    , tags :: Maybe [TagObject]
    , externalDocs :: Maybe ExternalDocumentationObject
    }
    deriving (Show, Eq, Generic)

instance FromJSON OpenApiObject
instance ToJSON OpenApiObject
instance Hashable OpenApiObject

data InfoObject = InfoObject
    { title :: String
    , description :: Maybe String
    , termsOfService :: Maybe String
    , contact :: Maybe ContactObject
    , license :: Maybe LicenseObject
    , version :: String
    -- extensions :: Map String Any
    }
    deriving (Show, Eq, Generic)

instance FromJSON InfoObject
instance ToJSON InfoObject
instance Hashable InfoObject

data ContactObject = ContactObject
    { name :: Maybe String
    , url :: Maybe String
    , email :: Maybe String
    }
    -- extensions :: Map String Any
    deriving (Show, Eq, Generic)

instance FromJSON ContactObject
instance ToJSON ContactObject
instance Hashable ContactObject

data LicenseObject = LicenseObject
    { name :: String
    , url :: Maybe String
    }
    -- extensions :: Map String Any
    deriving (Show, Eq, Generic)

instance FromJSON LicenseObject
instance ToJSON LicenseObject
instance Hashable LicenseObject

data ServerObject = ServerObject
    { url :: String
    , description :: Maybe String
    , variables :: Maybe (Map String ServerVariableObject)
    }
    deriving (Show, Eq, Generic)

instance FromJSON ServerObject
instance ToJSON ServerObject
instance Hashable ServerObject

data ServerVariableObject = ServerVariableObject
    { enum :: Maybe [String]
    , defaultValue :: String
    , description :: Maybe String
    }
    deriving (Show, Eq, Generic)

instance FromJSON ServerVariableObject where
    parseJSON = withObject "ServerVariableObject" $ \o -> do
        enum         <- o .:? "enum"
        defaultValue <- o .: "default"
        description  <- o .:? "description"
        return $ ServerVariableObject { .. }

instance ToJSON ServerVariableObject where
    toJSON ServerVariableObject {..} =
        object
            $ ("default" .= defaultValue)
            : (("enum" .=? enum) ++ ("description" .=? description))

instance Hashable ServerVariableObject

type RefOrSchemaObjectMap = Map String (ReferenceWith SchemaObject)
type RefOrResponseObjectMap = Map String (ReferenceWith ResponseObject)
type RefOrParameterObjectMap = Map String (ReferenceWith ParameterObject)
type RefOrExampleObjectMap = Map String (ReferenceWith ExampleObject)
type RefOrRequestBodyObjectMap = Map String (ReferenceWith RequestBodyObject)
type RefOrHeaderObjectMap = Map String (ReferenceWith HeaderObject)
type RefOrSecuritySchemesMap = Map String (ReferenceWith SecuritySchemeObject)
type RefOrLinkObjectMap = Map String (ReferenceWith LinkObject)
type RefOrCallbackObjectMap = Map String (ReferenceWith CallbackObject)

data ComponentsObject = ComponentsObject
    { schemas :: Maybe RefOrSchemaObjectMap
    , responses :: Maybe RefOrResponseObjectMap
    , parameters :: Maybe RefOrParameterObjectMap
    , examples :: Maybe RefOrExampleObjectMap
    , requestBodies :: Maybe RefOrRequestBodyObjectMap
    , headers :: Maybe RefOrHeaderObjectMap
    , securitySchemes :: Maybe RefOrSecuritySchemesMap
    , links :: Maybe RefOrLinkObjectMap
    , callbacks :: Maybe RefOrCallbackObjectMap
    }
    deriving (Show, Eq, Generic)

instance FromJSON ComponentsObject
instance ToJSON ComponentsObject
instance Hashable ComponentsObject

type PathsObject = Map Text PathItemObject
    -- deriving (Show, Eq, Generic)

    {-
instance FromJSON PathsObject where
    parseJSON = withObject "Paths object" $ \v -> do
        let key = Prelude.head . MS.keys $ v
        pathItem <- v .: key
        return $ PathsObject key pathItem
instance ToJSON PathsObject where
    toJSON (PathsObject key value) = object [key .= value]
-}

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
    , servers :: Maybe [ServerObject]
    , parameters :: Maybe [ReferenceWith ParameterObject]
    }
    deriving (Show, Eq, Generic)

instance FromJSON PathItemObject
instance ToJSON PathItemObject
instance Hashable PathItemObject

data OperationObject = OperationObject
    { tags :: Maybe [String]
    , summary ::  Maybe String
    , description :: Maybe Text
    , externalDocs :: Maybe ExternalDocumentationObject
    , operationId :: Maybe String
    , parameters :: Maybe [ReferenceWith ParameterObject]
    , requestBody :: Maybe (ReferenceWith RequestBodyObject)
    , responses :: ResponsesObject
    , callbacks :: Maybe (Map String (ReferenceWith CallbackObject))
    , deprecated :: Maybe Bool
    , security :: Maybe [SecurityRequirementObject]
    , servers :: Maybe [ServerObject]
    }
    deriving (Show, Eq, Generic)

instance FromJSON OperationObject
instance ToJSON OperationObject
instance Hashable OperationObject

data ExternalDocumentationObject = ExternalDocumentationObject
    { description :: Maybe String
    , url :: String
    }
    deriving (Show, Eq, Generic)

instance FromJSON ExternalDocumentationObject
instance ToJSON  ExternalDocumentationObject
instance Hashable  ExternalDocumentationObject

data InParameterObject
   = Query
   | Header
   | Path
   | Cookie
    deriving (Show, Eq, Generic)

instance FromJSON InParameterObject where
    parseJSON = withText "in options" $ \case
        "query"  -> return Query
        "header" -> return Header
        "Path"   -> return Path
        "cookie" -> return Cookie

instance ToJSON InParameterObject where
    toJSON Query  = "query"
    toJSON Header = "header"
    toJSON Path   = "path"
    toJSON Cookie = "cookie"

data ParameterObject = ParameterObject
    { name :: String
    , inLocation :: String
    , description :: Maybe String
    , required :: Maybe Bool
    , deprecated :: Maybe Bool
    , allowEmptyValue :: Maybe Bool
    , style :: Maybe String
    , explode :: Maybe Bool
    , allowReserved :: Maybe Bool

    , schema :: Maybe (ReferenceWith SchemaObject)
    , example :: Maybe Any
    , examples :: Maybe (Map String (ReferenceWith ExampleObject))
    , content :: Maybe (Map String MediaTypeObject)
    }
    deriving (Show, Eq, Generic)

instance FromJSON ParameterObject where
    parseJSON = withObject "Parameter Object" $ \o -> do
        name            <- o .: "name"
        inLocation      <- o .: "in"
        description     <- o .:? "description"
        required        <- o .:? "required"
        deprecated      <- o .:? "deprecated"
        allowEmptyValue <- o .:? "allowEmptyValue"
        style           <- o .:? "style"
        explode         <- o .:? "explode"
        allowReserved   <- o .:? "allowReserved"
        schema          <- o .:? "schema"
        example         <- o .:? "example"
        examples        <- o .:? "examples"
        content         <- o .:? "content"
        return $ ParameterObject { .. }

instance ToJSON ParameterObject where
    toJSON ParameterObject {..} =
        object $ ["name" .= name, "in" .= inLocation] ++ concat
            [ "description" .=? description
            , "required" .=? required
            , "deprecated" .=? deprecated
            , "allowEmptyValue" .=? allowEmptyValue
            , "style" .=? style
            , "explode" .=? explode
            , "allowReserved" .=? allowReserved
            , "schema" .=? schema
            , "example" .=? example
            , "examples" .=? examples
            , "content" .=? content
            ]

instance Hashable ParameterObject

data RequestBodyObject = RequestBodyObject
    { description :: Maybe String
    , content :: Map String MediaTypeObject
    , required :: Maybe Bool
    }
    deriving (Show, Eq, Generic)

instance FromJSON RequestBodyObject
instance ToJSON RequestBodyObject
instance Hashable RequestBodyObject

data MediaTypeObject = MediaTypeObject
    { schema :: Maybe (ReferenceWith SchemaObject)
    , example :: Maybe Any
    , examples :: Maybe (Map String (ReferenceWith ExampleObject))
    , encoding :: Maybe (Map String EncodingObject)
    }
    deriving (Show, Eq, Generic)

instance FromJSON MediaTypeObject
instance ToJSON MediaTypeObject
instance Hashable MediaTypeObject

data EncodingObject = EncodingObject
    { contentType :: Maybe String
    , headers :: Maybe (Map String (ReferenceWith HeaderObject))
    , style :: Maybe String
    , explode :: Maybe Bool
    , allowReserved :: Maybe Bool
    }
    deriving (Show, Eq, Generic)

instance FromJSON EncodingObject
instance ToJSON EncodingObject
instance Hashable EncodingObject

newtype ResponsesObject = ResponsesObject (Map String (ReferenceWith ResponseObject))
    deriving (Show, Eq, Generic)

instance FromJSON ResponsesObject
instance ToJSON ResponsesObject
instance Hashable ResponsesObject

data ResponseObject = ResponseObject
    { description :: String
    , headers :: Maybe (Map String (ReferenceWith HeaderObject))
    , content :: Maybe (Map String MediaTypeObject)
    , links :: Maybe (Map String (ReferenceWith LinkObject))
    }
    deriving (Show, Eq, Generic)

instance FromJSON ResponseObject
instance ToJSON ResponseObject
instance Hashable ResponseObject

newtype CallbackObject
    = CallbackObject { callbackObject :: Maybe (Map String PathItemObject) }
    deriving (Show, Eq, Generic)

instance FromJSON CallbackObject
instance ToJSON CallbackObject
instance Hashable CallbackObject

data ExampleObject = ExampleObject
    { summary :: Maybe String
    , description :: Maybe String
    , value :: Maybe Any
    , externalValue :: Maybe String
    }
    deriving (Show, Eq, Generic)

instance FromJSON ExampleObject
instance ToJSON ExampleObject
instance Hashable ExampleObject

-- TODO Validate the String Expression
{-
expression = ( "$url" | "$method" | "$statusCode" | "$request." source | "$response." source )
source = ( header-reference | query-reference | path-reference | body-reference )  
header-reference = "header." token
query-reference = "query." name  
path-reference = "path." name
body-reference = "body" ["#" fragment]
fragment = a JSON Pointer [RFC 6901](https://tools.ietf.org/html/rfc6901)  
name = *( char )
char = as per RFC [7159](https://tools.ietf.org/html/rfc7159#section-7)
token = as per RFC [7230](https://tools.ietf.org/html/rfc7230#section-3.2.6)
-}
type Expression = String

data LinkObject = LinkObject
    { operationRef :: Maybe String
    , operationId :: Maybe String
    -- , parameters :: Maybe (Map String (Either Any Expression))
    , parameters :: Maybe (Map String Any)
    -- , requestBody :: Maybe (Either Any Expression)
    , requestBody :: Maybe Any
    , description :: Maybe String
    , server :: Maybe ServerObject
    }
    deriving (Show, Eq, Generic)

instance FromJSON LinkObject
instance ToJSON LinkObject
instance Hashable LinkObject

data HeaderObject = HeaderObject
    { description :: Maybe String
    , required :: Maybe Bool
    , deprecated :: Maybe Bool
    , allowEmptyValue :: Maybe Bool
    , style :: Maybe String
    , explode :: Maybe Bool
    , allowReserved :: Maybe Bool

    , schema :: Maybe (ReferenceWith SchemaObject)
    , example :: Maybe Any
    , examples :: Maybe (Map String (ReferenceWith ExampleObject))
    , content :: Maybe (Map String MediaTypeObject)
    }
    deriving (Show, Eq, Generic)

instance FromJSON HeaderObject
instance ToJSON HeaderObject
instance Hashable HeaderObject

data TagObject = TagObject
    { name :: String
    , description :: Maybe String
    , externalDocs :: Maybe ExternalDocumentationObject
    }
    deriving (Show, Eq, Generic)

instance FromJSON TagObject
instance ToJSON TagObject
instance Hashable TagObject

newtype ReferenceObject
    = ReferenceObject { ref :: String }
    deriving (Show, Eq, Generic)

instance FromJSON ReferenceObject where
    parseJSON = withObject "Reference" $ \o -> ReferenceObject <$> o .: "$ref"

instance ToJSON ReferenceObject where
    toJSON ReferenceObject {..} = object [("$ref", toJSON ref)]

instance Hashable ReferenceObject

data NumberSchemaOptions t f
    = MultipleOf { multipleOf :: t }
    | Minimum { minimum :: t }
    | Maximum { maximum :: t }
    | ExclusiveMinimum { exclusiveMinimum :: t }
    | ExclusiveMaximum { exclusiveMaximum :: t }
    | DefaultNumber { defaultValue :: t }
    | EnumNumber { enum :: [t] }
    | NumberFormat { format :: f }
    deriving (Show, Eq, Generic)

instance (ToJSON t, ToJSON f) => ToJSON (NumberSchemaOptions t f) where
    toJSON (MultipleOf       n ) = object ["multipleOf" .= n]
    toJSON (Minimum          n ) = object ["minimum" .= n]
    toJSON (Maximum          n ) = object ["maximum" .= n]
    toJSON (ExclusiveMinimum n ) = object ["exclusiveMinimum" .= n]
    toJSON (ExclusiveMaximum n ) = object ["exclusiveMaximum" .= n]
    toJSON (NumberFormat     f ) = object ["format" .= toJSON f]
    toJSON (DefaultNumber    n ) = object ["default" .= n]
    toJSON (EnumNumber       ns) = object ["enum" .= ns]

instance (Hashable t, Hashable f) => Hashable (NumberSchemaOptions t f)


data StringSchemaOptions
    = MinLength { minLength :: Int }
    | MaxLength { maxLength :: Int }
    | PatternValue { patternValue :: String }
    | StringFormat { format :: String}
    | DefaultString { defaultValue :: String }
    | EnumString { enum :: [String] }
    deriving (Show, Eq, Generic)

parseStringSchemaType :: Value -> Parser SchemaType
parseStringSchemaType = withObject "String" $ \o -> do
    minLengthL     <- map MinLength <$> o .::? "minLength"
    maxLengthL     <- map MaxLength <$> o .::? "maxLength"
    patternValueL  <- map PatternValue <$> o .::? "pattern"
    defaultStringL <- map DefaultString <$> o .::? "default"
    enumStringL    <- map EnumString <$> o .::? "enum"
    stringFormatL  <- map StringFormat <$> o .::? "format"
    return
        . StringSchemaType
        . concat
        $ [ minLengthL
          , maxLengthL
          , defaultStringL
          , enumStringL
          , patternValueL
          , stringFormatL
          ]

instance ToJSON StringSchemaOptions where
    toJSON MinLength {..}     = object ["minLength" .= minLength]
    toJSON MaxLength {..}     = object ["maxLength" .= maxLength]
    toJSON PatternValue {..}  = object ["pattern" .= patternValue]
    toJSON StringFormat {..}  = object ["format" .= format]
    toJSON DefaultString {..} = object ["default" .= defaultValue]
    toJSON EnumString {..}    = object ["enum" .= enum]

instance Hashable StringSchemaOptions

data ArraySchemaOption
    = MinItems { minItems :: Int}
    | MaxItems { maxItems :: Int}
    | UniqueItems { uniqueItems :: Bool}
    | Items { items :: ReferenceWith SchemaObject}
    deriving (Show, Eq, Generic)

parseArraySchemaType :: Value -> Parser SchemaType
parseArraySchemaType = withObject "Array" $ \o -> do
    minItemsL    <- map MinItems <$> o .::? "minItems"
    maxItemsL    <- map MaxItems <$> o .::? "maxItems"
    uniqueItemsL <- map UniqueItems <$> o .::? "uniqueItems"
    itemsL       <- map Items <$> o .::? "items"
    return
        . ArraySchemaType
        . concat
        $ [minItemsL, maxItemsL, uniqueItemsL, itemsL]

instance FromJSON ArraySchemaOption

instance ToJSON ArraySchemaOption where
    toJSON MinItems {..}    = object ["minItems" .= minItems]
    toJSON MaxItems {..}    = object ["maxItems" .= maxItems]
    toJSON UniqueItems {..} = object ["uniqueItems" .= uniqueItems]
    toJSON Items {..}       = object ["items" .= items]

instance Hashable ArraySchemaOption

data ObjectSchemaOptions
    = MinProperties { minProperties :: Int}
    | MaxProperties { maxProperties :: Int}
    | Required { required :: [String]}
    deriving (Show, Eq, Generic)

instance FromJSON ObjectSchemaOptions

instance ToJSON ObjectSchemaOptions where
    toJSON MinProperties {..} = object ["minItems" .= minProperties]
    toJSON MaxProperties {..} = object ["maxItems" .= maxProperties]
    toJSON Required {..}      = object ["required" .= required]

instance Hashable ObjectSchemaOptions

data SchemaType
    = StringSchemaType [StringSchemaOptions]
    | ObjectSchemaType
        { properties :: Map String SchemaObject
        , options :: [ObjectSchemaOptions]
        , additionalProperties :: Either Bool (ReferenceWith SchemaObject)
        }
    | ArraySchemaType [ArraySchemaOption]
    | BooleanSchemaType
    | NullSchemaType
    | IntegerSchemaType [NumberSchemaOptions Int IntegerFormat]
    | NumberSchemaType [NumberSchemaOptions Double NumberFormat]
    | AllOfSchemaType { allOf :: [SchemaObject], discriminator :: Maybe DiscriminatorObject}
    | OneOfSchemaType { oneOf :: [SchemaObject], discriminator :: Maybe DiscriminatorObject}
    | AnyOfSchemaType { anyOf :: [SchemaObject], discriminator :: Maybe DiscriminatorObject}
    | NotSchemaType { not:: SchemaObject}
    | ReferenceSchemaType ReferenceObject
    deriving (Show, Eq, Generic)

instance ToJSON SchemaType where
    toJSON (StringSchemaType options) =
        object
            $ ("type", String "string")
            : (options >>= convertJObjectToPairList . toJSON)
    toJSON ObjectSchemaType {..} =
        object
            $  ("type"      , String "object")
            :  ("properties", toJSON properties)
            :  (options >>= convertJObjectToPairList . toJSON)
            ++ addProperties additionalProperties
      where
        addProperties (Left  True ) = []
        addProperties (Left  False) = [("additionalProperties", toJSON False)]
        addProperties (Right props) = [("additionalProperties", toJSON props)]
    toJSON (ArraySchemaType options) =
        object
            $ ("type", String "array")
            : (options >>= convertJObjectToPairList . toJSON)
    toJSON BooleanSchemaType = object [("type", String "boolean")]
    toJSON NullSchemaType    = object [("type", String "null")]
    toJSON (IntegerSchemaType options) =
        object
            $ ("type", String "integer")
            : (options >>= convertJObjectToPairList . toJSON)
    toJSON (NumberSchemaType options) =
        object
            $ ("type", String "number")
            : (options >>= convertJObjectToPairList . toJSON)
    toJSON AllOfSchemaType {..} =
        object
            $ ("allOf", toJSON allOf)
            : maybe [] (\disc -> [("discriminator", toJSON disc)]) discriminator
    toJSON OneOfSchemaType {..} =
        object
            $ ("oneOf", toJSON oneOf)
            : maybe [] (\disc -> [("discriminator", toJSON disc)]) discriminator
    toJSON AnyOfSchemaType {..} =
        object
            $ ("anyOf", toJSON anyOf)
            : maybe [] (\disc -> [("discriminator", toJSON disc)]) discriminator
    toJSON NotSchemaType {..}        = object [("not", toJSON not)]
    toJSON (ReferenceSchemaType ref) = toJSON ref

instance Hashable SchemaType


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
    formatL           <- map NumberFormat <$> o .::? "format"
    return
        . ctr
        . concat
        $ [ multipleOfL
          , minimumL
          , maximumL
          , exclusiveMinimumL
          , exclusiveMaximumL
          , formatL
          ]


parseObjectOptions :: Object -> Parser [ObjectSchemaOptions]
parseObjectOptions o = do
    minPropertiesL <- map MinProperties <$> o .::? "minProperties"
    maxPropertiesL <- map MaxProperties <$> o .::? "maxProperties"
    requiredL      <- map Required <$> o .::? "required"
    return . concat $ [minPropertiesL, maxPropertiesL, requiredL]


parseObjectSchemaType = withObject "Object" $ \o -> do
    properties           <- o .: "properties"
    options              <- parseObjectOptions o
    additionalProperties <-
        fromMaybe (Left True) <$> o .:? "additionalProperties"
    return ObjectSchemaType { .. }

parseSimpleType :: String -> Value -> Parser SchemaType
parseSimpleType str@"integer" = parseNumberSchemaType str IntegerSchemaType
parseSimpleType str@"number"  = parseNumberSchemaType str NumberSchemaType
parseSimpleType "string"      = parseStringSchemaType
parseSimpleType "array"       = parseArraySchemaType
parseSimpleType "boolean"     = \_ -> return BooleanSchemaType
parseSimpleType "null"        = \_ -> return NullSchemaType
parseSimpleType "object"      = parseObjectSchemaType
parseSimpleType tt            = \_ -> fail $ "Invalid type '" ++ tt ++ "'"

data SchemaObject = SchemaObject
     -- Json Schema Object derived
    { schemaType :: SchemaType
    , defaultValue :: Maybe Value
    , enum :: Maybe [Value]
    , title :: Maybe String
    , description :: Maybe String
    , nullable :: Maybe Bool
    , readOnly :: Maybe Bool
    , writeOnly :: Maybe Bool
    , xml :: Maybe XMLObject
    , externalDocs :: Maybe ExternalDocumentationObject
    , example :: Maybe Any
    , deprecated :: Maybe Bool
    }
    deriving (Show, Eq, Generic)

instance FromJSON SchemaObject where
    parseJSON val = withObject
        "SchemaObject"
        (\o -> do
            title        <- o .:? "title"
            defaultValue <- o .:? "default"
            enum         <- o .:? "enum"
            description  <- o .:? "description"
            nullable     <- o .:? "nullable"
            readOnly     <- o .:? "readOnly"
            writeOnly    <- o .:? "writeOnly"
            xml          <- o .:? "xml"
            externalDocs <- o .:? "externalDocs"
            example      <- o .:? "example"
            deprecated   <- o .:? "deprecated"
            schemaType   <- parseSchemaType o val
            return $ SchemaObject { .. }
        )
        val

instance ToJSON SchemaObject where
    toJSON SchemaObject {..} = toJSON schemaType `objectsMerge` rest
      where
        rest = object $ concat
            [ "title" .=? title
            , "default" .=? defaultValue
            , "enum" .=? enum
            , "description" .=? description
            , "nullable" .=? nullable
            , "readOnly" .=? readOnly
            , "writeOnly" .=? writeOnly
            , "xml" .=? xml
            , "externalDocs" .=? externalDocs
            , "example" .=? example
            , "deprecated" .=? deprecated
            ]

instance Hashable SchemaObject

parseSchemaType o val =
    (OneOfSchemaType <$> o .: "oneOf" <*> o .:? "discriminator")
        <|> (AllOfSchemaType <$> o .: "allOf" <*> o .:? "discriminator")
        <|> (AnyOfSchemaType <$> o .: "anyOf" <*> o .:? "discriminator")
        <|> (NotSchemaType <$> o .: "not")
        <|> (ReferenceSchemaType <$> parseJSON val)
        <|> (o .: "type" >>= \theType -> parseSimpleType theType val)

convertJObjectToPairList :: Value -> [(T.Text, Value)]
convertJObjectToPairList (Object obj) = MS.toList obj

objectsMerge :: Value -> Value -> Value
objectsMerge (Object a) (Object b) = Object (MS.union a b)
objectsMerge _ _ = error "Objects merge can only merge json objects"

data DiscriminatorObject = DiscriminatorObject
    { propertyName :: String
    , mappint :: Maybe (Map String String)
    }
    deriving (Show, Eq, Generic)

instance FromJSON DiscriminatorObject

instance ToJSON DiscriminatorObject

instance Hashable DiscriminatorObject

data XMLOptions
    = NameOption { name :: String }
    | Namespace {namespace :: String }
    | Prefix { prefix :: String }
    | Attribute { attribute :: Bool }
    | Wrapped { wrapped :: Bool }
    deriving (Show, Eq, Generic)

instance FromJSON XMLOptions

instance Hashable XMLOptions


pairXMLOptions (NameOption name) = ("name", toJSON name)
pairXMLOptions (Namespace namespace) = ("namespace", toJSON namespace)
pairXMLOptions (OpenApi3.Models.Prefix prefix) = ("prefix", toJSON prefix)
pairXMLOptions (Attribute attribute) = ("attribute", toJSON attribute)
pairXMLOptions (Wrapped wrapped) = ("wrapped", toJSON wrapped)

newtype XMLObject = XMLObject [XMLOptions]
    deriving (Show, Eq, Generic)

(.::?) :: FromJSON a => Object -> Text -> Parser [a]
o .::? field = maybeToList <$> o .:? field

instance FromJSON XMLObject where
    parseJSON = withObject
        "XMLObject"
        (\o -> do
            nameL      <- map NameOption <$> o .::? "name"
            namespaceL <- map Namespace <$> o .::? "namespace"
            prefixL    <- map OpenApi3.Models.Prefix <$> o .::? "prefix"
            attributeL <- map Attribute <$> o .::? "attribute"
            wrappedL   <- map Wrapped <$> o .::? "wrapped"
            return
                . XMLObject
                . concat
                $ [nameL, namespaceL, prefixL, attributeL, wrappedL]
        )

instance ToJSON XMLObject where
    toJSON (XMLObject options) = object $ map pairXMLOptions options

instance Hashable XMLObject

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
        object $ "type" .= securitySchemeType : concat
            [ "description" .=? description
            , "name" .=? name
            , "in" .=? securitySchemeIn
            , "scheme" .=? scheme
            , "bearerFormat" .=? bearerFormat
            , "flows" .=? flows
            , "openIdConnectUrl" .=? openIdConnectUrl
            ]

instance Hashable SecuritySchemeObject

data SecuritySchemeObjectType
    = ApiKey
    | Http
    | OAuth2
    | OpenIdConnect
    deriving (Show, Eq, Generic)

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

instance Hashable SecuritySchemeObjectType

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

instance Hashable OAuthFlowsObject

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

instance Hashable OAuthFlowObject

type SecurityRequirementObject = Map String [String]

type SpecificationExtensions = Map String Any


