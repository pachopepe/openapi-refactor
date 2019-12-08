{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module OpenApi3.Refactor where

import           OpenApi3.Models
import           Control.Monad.State.Lazy      as St
import qualified Data.Map.Lazy                 as M
import           Data.Maybe
import qualified Data.Text                     as T
import           Data.Hashable
import           Data.List                      ( nub )


data Info a = Info { infoKey:: T.Text, info:: ReferenceWith a}
    deriving (Show, Eq)

data Dict = Dict
    { theSchemas :: M.Map Int [Info SchemaObject]
    , theParameters :: M.Map Int [Info ParameterObject]
    , theExamples :: M.Map Int [Info ExampleObject]
    -- , responsesD :: RefOrResponseObjectMap
    -- , parametersD :: RefOrParameterObjectMap
    -- , examplesD :: RefOrExampleObjectMap
    -- , requestBodiesD :: Maybe RefOrRequestBodyObjectMap
    -- , headersD :: RefOrHeaderObjectMap
    -- , securitySchemesD :: RefOrSecuritySchemesMap
    -- , linksD :: RefOrLinkObjectMap
    -- , callbacksD :: RefOrCallbackObjectMap
    }

type RState = State Dict

factorizeWithReference
    :: (Hashable a, Eq a)
    => (Dict -> M.Map Int [Info a])
    -> ReferenceWith a
    -> RState (ReferenceWith a)
factorizeWithReference _ ref@(Reference _) = return ref
factorizeWithReference f v                 = do
    mp <- f <$> St.get
    if hash v `M.member` mp
        then case filter (\Info {..} -> info == v) $ mp M.! hash v of
            []          -> return v
            [Info {..}] -> return $ getReference infoKey
            values ->
                fail
                    $ "Factorization error in findInfo, not unique value in map : "
                    ++ show (map (T.unpack . infoKey) values)
        else return v

mapToInfoMap
    :: (Eq a, Hashable a)
    => M.Map T.Text (ReferenceWith a)
    -> M.Map Int [Info a]
mapToInfoMap = M.foldrWithKey upsert M.empty

infoToMap :: M.Map Int [Info a] -> M.Map T.Text (ReferenceWith a)
infoToMap =
    M.foldr (flip (foldr (\Info {..} -> M.insert infoKey info))) M.empty

upsert
    :: (Hashable a, Eq a)
    => T.Text
    -> ReferenceWith a
    -> M.Map Int [Info a]
    -> M.Map Int [Info a]
upsert k v mp | hash v `M.member` mp =
    case filter (\Info {..} -> info == v) $ mp M.! hash v of
        []                               -> add k v mp
        [Info { info = Reference _, ..}] -> add k v mp
        [Info {..}                     ] -> add k (getReference infoKey) mp
        values ->
            error
                $  "Factorization error in upsert, not unique values in map: "
                ++ show (map (T.unpack . infoKey) values)
upsert k v mp = add k v mp

add k v = M.insertWith (++) (hash v) [Info k v]

-- TODO Review the reference format
getReference :: T.Text -> ReferenceWith a
getReference name =
    Reference (ReferenceObject $ "#components/schemas/" <> name)

factorizeMaybe :: (a -> RState a) -> Maybe a -> RState (Maybe a)
factorizeMaybe _ Nothing = return Nothing
factorizeMaybe f (Just a) = Just <$> f a

factorizeOpenApi :: OpenApiObject -> RState OpenApiObject
factorizeOpenApi OpenApiObject {..} = do
    components <- factorizeMaybe factorizeComponentsObject components
    paths <- mapM factorizePathItemObject paths
    return OpenApiObject { .. }

factorizeComponentsObject :: ComponentsObject -> RState ComponentsObject
factorizeComponentsObject ComponentsObject {..} = do
    Dict {..} <- St.get
    -- Global definitions
    let theSchemas    = maybe theSchemas mapToInfoMap schemas
        theParameters = maybe theParameters mapToInfoMap parameters
        theExamples = maybe theExamples mapToInfoMap examples
    St.put $ Dict { .. }
    let schemas    = mMap theSchemas
        parameters = mMap theParameters
        examples = mMap theExamples
    return $ ComponentsObject { .. }
  where
    mMap d | M.null d  = Nothing
           | otherwise = Just (infoToMap d)

factorizePathItemObject :: PathItemObject -> RState PathItemObject
factorizePathItemObject PathItemObject{..} = do
    get <- factorizeMaybe factorizeOperation get
    put <- factorizeMaybe factorizeOperation put
    post <- factorizeMaybe factorizeOperation post
    delete <- factorizeMaybe factorizeOperation delete
    options <- factorizeMaybe factorizeOperation options
    head <- factorizeMaybe factorizeOperation head
    patch <- factorizeMaybe factorizeOperation patch
    trace <- factorizeMaybe factorizeOperation trace
    parameters <- factorizeMaybe (mapM factorizeParameter) parameters
    return PathItemObject{..}
    
factorizeOperation :: OperationObject -> RState OperationObject
factorizeOperation OperationObject {..} = do
    parameters <- factorizeMaybe (mapM factorizeParameter) parameters
    return OperationObject { .. }

factorizeParameter :: ReferenceWith ParameterObject -> RState (ReferenceWith ParameterObject)
factorizeParameter param = do
    param' <- factorizeWithReference theParameters param 
    case param' of
      Inline ParameterObject{..} -> do 
        schema <- factorizeMaybe factorizeSchema schema
        return $ Inline ParameterObject {..}
      ref -> return ref

factorizeSchema :: ReferenceWith SchemaObject -> RState (ReferenceWith SchemaObject)
factorizeSchema param = do
    param' <- factorizeWithReference theSchemas param 
    case param' of
      Inline SchemaObject{..} -> do
          -- TODO take in account the properties when the type is an object
          return $ Inline SchemaObject{..}
      ref -> return ref 

factorizeExample :: ReferenceWith ExampleObject -> RState (ReferenceWith ExampleObject)
factorizeExample param = do
    param' <- factorizeWithReference theExamples param 
    case param' of
      Inline ExampleObject{..} -> do
          -- TODO take in account the properties when the type is an object
          return $ Inline ExampleObject{..}
      ref -> return ref 


    

    
