{-# LANGUAGE RecordWildCards #-}

module OpenApi3.Refactor where

import           OpenApi3.Models
import           Control.Monad.State.Lazy      as S
import qualified Data.Map.Lazy                 as M

data Dict = Dict
    { schemasD :: RefOrSchemaObjectMap
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

factorizeSchemaComponentsObject :: ComponentsObject -> RState ComponentsObject
factorizeSchemaComponentsObject ComponentsObject {..} = do
    Dict { schemasD = schemaMap } <- S.get

    return $ ComponentsObject { .. }


-- refactor :: OpenApiObject -> (State, OpenApiObjec)
-- refactor

