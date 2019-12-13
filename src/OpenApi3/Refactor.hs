{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module OpenApi3.Refactor where

import           OpenApi3.Models
import           Control.Monad.State.Lazy      as St
import qualified Data.Map.Lazy                 as M
import qualified Data.Set                     as S
import           Data.Maybe
import qualified Data.Text                     as T
import           Data.Hashable
import           Data.List                      ( nub, find )
import Debug.Trace


data Info a = Info { infoKey:: T.Text, info:: ReferenceWith a, count :: Int, isGlobal:: Bool }
    deriving (Show, Eq)

data Dict = Dict
    { getSchemas :: (M.Map Int [Info SchemaObject], S.Set T.Text)
    , getParameters :: (M.Map Int [Info ParameterObject], S.Set T.Text)
    , getExamples :: (M.Map Int [Info ExampleObject], S.Set T.Text)
    -- , responsesD :: RefOrResponseObjectMap
    -- , requestBodiesD :: Maybe RefOrRequestBodyObjectMap
    -- , headersD :: RefOrHeaderObjectMap
    -- , securitySchemesD :: RefOrSecuritySchemesMap
    -- , linksD :: RefOrLinkObjectMap
    -- , callbacksD :: RefOrCallbackObjectMap
    }

setSchemas :: (M.Map Int [Info SchemaObject], S.Set T.Text) -> Dict -> Dict
setSchemas mp dict = dict {getSchemas = mp}

setParameters :: (M.Map Int [Info ParameterObject], S.Set T.Text) -> Dict -> Dict
setParameters mp dict = dict {getParameters = mp}

setExamples :: (M.Map Int [Info ExampleObject], S.Set T.Text) -> Dict -> Dict
setExamples mp dict = dict {getExamples = mp}

type RState = State Dict

factorizeWithReference
    :: (Hashable a, Eq a)
    => T.Text -> T.Text 
    -> (Dict -> (M.Map Int [Info a], S.Set T.Text))
    -> ((M.Map Int [Info a], S.Set T.Text) -> Dict -> Dict)
    -> ReferenceWith a
    -> RState (ReferenceWith a)
factorizeWithReference _ _ _ _ ref@(Reference _) = return ref
factorizeWithReference baseName keyName getDict setDict v                 = do
    dict <- St.get
    let (mp,ss) = getDict dict
    if hash v `M.member` mp
    then case filter (\Info {..} -> info == v) $ mp M.! hash v of
        []          -> 
            -- TODO how to improve unique inline references
            return v
        [Info {..}] -> return $ getReference baseName infoKey
        values ->
            fail
                $ "Factorization error in findInfo, not unique value in map : "
                ++ show (map (T.unpack . infoKey) values)
    else return v

isReference (Reference _) = True
isReference _ = False

addToDictionary
    :: (Hashable a, Eq a)
    => Bool 
    -> T.Text
    -> T.Text 
    -> (Dict -> (M.Map Int [Info a], S.Set T.Text))
    -> ((M.Map Int [Info a], S.Set T.Text) -> Dict -> Dict)
    -> ReferenceWith a
    -> RState (ReferenceWith a)
addToDictionary _ _ _ _ _ ref@(Reference _) = return ref
addToDictionary isGlobal baseName keyName getDict setDict v = do
    dict <- St.get
    let (mp,ss) = getDict dict
        h = hash v
        elems = if h `M.member` mp then mp M.! h else []
        keys = [ infoKey  | Info {..} <- elems, info == v ]
    if null keys || keyName `Prelude.notElem` keys
        then do
                -- TODO how to improve unique inline references
                let k' = getUniqueName ss keyName
                    elems' = Info k' v 1 isGlobal : elems
                    mp' = M.insert h elems' mp
                    ss' = S.insert k' ss
                St.put $ setDict (mp',ss') dict
                return $ getReference baseName k'
        else    do
                let Just Info{..} = find (\Info{..} -> info == v && infoKey == keyName) elems
                    elems' = Info {count = count + 1,..} : elems
                    mp' = M.insert h elems' mp
                St.put $ setDict (mp',ss) dict
                if isReference v
                then return v
                else return $ getReference baseName keyName

mapToInfoMap
    :: (Eq a, Hashable a)
    => T.Text
    -> M.Map T.Text (ReferenceWith a)
    -> (M.Map Int [Info a], S.Set T.Text)
    -> (M.Map Int [Info a], S.Set T.Text)
mapToInfoMap baseName mp init = M.foldrWithKey (upsert True baseName) init mp

infoToMap :: M.Map Int [Info a] -> M.Map T.Text (ReferenceWith a)
infoToMap =
    M.foldr (flip (foldr (\Info {..} mp -> if isGlobal || True
                                        then M.insert infoKey info mp
                                        else mp))) M.empty

getUniqueName ss nm =
   if nm `S.member` ss
    then getUniqueName' 0
    else nm
  where getUniqueName' i =
          if newName `S.member` ss
          then getUniqueName' (i+1)
          else newName
         where newName = nm <> "_" <> (T.pack . show $ i) 

upsert
    :: (Hashable a, Eq a)
    => Bool
    -> T.Text
    -> T.Text
    -> ReferenceWith a
    -> (M.Map Int [Info a],S.Set T.Text)
    -> (M.Map Int [Info a],S.Set T.Text)
upsert isGlobal baseName k v (mp,ss) | h `M.member` mp =
    case filter (\Info {..} -> info == v) elems of
        []                               -> snd $ add k v 1 isGlobal mp ss
        [Info { info = Reference _, ..}] -> if infoKey == k 
                                            then (mp',ss)
                                            else snd $ add k v 1 isGlobal mp ss
        [Info {..}                     ] -> if infoKey == k 
                                            then (mp',ss) 
                                            else snd $ add k (getReference baseName infoKey) 1 isGlobal mp ss
        values ->
            error
                $  "Factorization error in insert, not unique values in map: "
                ++ show (map (T.unpack . infoKey) values)
  where elems =  mp M.! h
        h = hash v 
        mp' = M.adjust upd h mp
        upd = map (\inf@Info {..} -> if k == infoKey && info == v then Info {count = count + 1,..} else inf) 
upsert isGlobal baseName k v (mp,ss) = snd $ add k v 1 isGlobal mp ss

add k v c global mp ss = (k',(M.insertWith (++) (hash v) [Info k' v c global] mp,S.insert k' ss))
    where k' = getUniqueName ss k 

-- TODO Review the reference format
getReference :: T.Text -> T.Text -> ReferenceWith a
getReference baseName name =
    Reference (ReferenceObject $ baseName <> "/" <> name)

factorizeMaybe :: (a -> RState a) -> Maybe a -> RState (Maybe a)
factorizeMaybe _ Nothing = return Nothing
factorizeMaybe f (Just a) = Just <$> f a

factorizeOpenApi :: OpenApiObject -> RState OpenApiObject
factorizeOpenApi OpenApiObject {..} = do
    let tags' = nub <$> tags
    components'<- factorizeMaybe factorizeComponentsObject components
    paths'<- mapM factorizePathItemObject paths
    return OpenApiObject {tags = tags', components = components', paths = paths', .. }

factorizeComponentsObject :: ComponentsObject -> RState ComponentsObject
factorizeComponentsObject ComponentsObject {..} = do
    Dict {..} <- St.get
    -- Global definitions
    let getSchemas'    = maybe getSchemas (flip (mapToInfoMap "#/components/schemas") getSchemas) schemas
        getParameters' = maybe getParameters (flip (mapToInfoMap "#/components/parameters") getParameters) parameters
        getExamples' = maybe getExamples (flip (mapToInfoMap "#/components/examples") getExamples) examples
    St.put Dict {getSchemas = getSchemas', getParameters = getParameters', getExamples = getExamples', .. }
    let schemas'    = convertInfoMap getSchemas'
        parameters' = convertInfoMap getParameters'
        examples' = convertInfoMap getExamples'
    return ComponentsObject {schemas = schemas', parameters = parameters', examples = examples', .. }
  where
    convertInfoMap (d,_) | M.null d  = Nothing
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
    param' <- factorizeWithReference "#/components/parameters" (getParameterName param) getParameters setParameters param
    case param' of
      Inline ParameterObject{..} -> do 
        schema' <- factorizeMaybe (factorizeSchema "GenericSchema") schema
        return $ Inline ParameterObject {schema = schema', ..}
      ref -> return ref
   where getParameterName (Inline ParameterObject {..}) = name
         getParameterName _ = "GenericParameter"

isRefOrBasicSchema (Reference _) = True
isRefOrBasicSchema (Inline sch@SchemaObject{..}) =
    case schemaType of
        StringSchemaType options -> null options && basicSchema sch
        BooleanSchemaType -> basicSchema sch
        NullSchemaType -> basicSchema sch
        IntegerSchemaType options -> null options && basicSchema sch
        NumberSchemaType options -> null options && basicSchema sch
        ReferenceSchemaType _ -> basicSchema sch
        _ -> False
  where basicSchema SchemaObject
            { defaultValue = Nothing
            , enum = Nothing
            , title = Nothing
            , description = Nothing
            , nullable = Nothing
            , readOnly = Nothing
            , writeOnly = Nothing
            , xml = Nothing
            , externalDocs = Nothing
            , example = Nothing
            , deprecated = Nothing
            } = True
        basicSchema _ = False

factorizeSchema :: T.Text -> ReferenceWith SchemaObject -> RState (ReferenceWith SchemaObject)
factorizeSchema schemaName param = do
    param' <- if isRefOrBasicSchema param
              then return param
              else factorizeWithReference "#/components/schemas" schemaName getSchemas setSchemas param 
    case param' of
      Inline SchemaObject{..} -> do
          -- TODO take in account the properties when the type is an object
          schemaType' <- 
                  case schemaType of
                    ObjectSchemaType{..} -> do
                        properties' <- M.fromList
                                       <$> mapM (\(k,v) -> (k,) <$> factorizeSchema k v) (M.toList properties)
                        return ObjectSchemaType {properties = properties', ..}
                    _ -> return schemaType
          return $ Inline SchemaObject{schemaType = schemaType', ..}
      ref -> return ref 

factorizeExample :: ReferenceWith ExampleObject -> RState (ReferenceWith ExampleObject)
factorizeExample param = do
    param' <- factorizeWithReference "#/components/examples" "GenericExample" getExamples setExamples param 
    case param' of
      Inline ExampleObject{..} -> do
          -- TODO take in account the properties when the type is an object
          return $ Inline ExampleObject{..}
      ref -> return ref 
