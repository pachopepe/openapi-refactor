{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}


module Example where

import           Data.Yaml
import           Control.Monad
import           Control.Applicative
import           GHC.Generics


data IntegerFormat
    = Int32
    | Int64
    deriving (Show, Eq, Generic)

newtype Elem = Elem
    { elem :: IntegerFormat
    }
    deriving (Show, Eq, Generic)

instance FromJSON IntegerFormat where
    parseJSON = withText "IntegerFormat" parse
      where
        parse "int32" = pure Int32
        parse "int64" = pure Int64
        parse s       = fail $ "Expected an IntegerFormat instead of " ++ show s

-- Y.decode "int32" :: Either String [IntegerFormat ]


-- instance FromYAML Elem where
--     parseYAML = withMap "elem" $ \m -> Elem <$> m .: "elem"
--
instance FromJSON Elem

--
--  Y.decode "elem: int2" :: Either String [Elem] 
