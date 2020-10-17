{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | This module contains the 'Generator' monad and functions which deal with this monad.
-- In addition this module contains the means for logging and resolving references since they are
-- closely linked to the 'Generator' monad.
module OpenAPI.Generate.Monad where

import qualified Control.Monad.Reader as MR
import qualified Control.Monad.Writer as MW
import Data.Text (Text)
import qualified OpenAPI.Generate.Log as OAL
import qualified OpenAPI.Generate.OptParse as OAO
import qualified OpenAPI.Generate.Reference as Ref
import qualified OpenAPI.Generate.Types as OAT
import qualified OpenAPI.Generate.Types.GeneratorConfiguration as OAG
import qualified OpenAPI.Generate.Types.Schema as OAS

-- | The reader environment of the 'Generator' monad
--
-- The 'currentPath' is updated using the 'nested' function to track the current position within the specification.
-- This is used to produce tracable log messages.
-- The 'references' map is a lookup table for references within the OpenAPI specification.
data GeneratorEnvironment = GeneratorEnvironment
  { currentPath :: [Text],
    references :: Ref.ReferenceMap,
    options :: OAO.Options,
    generatorConfiguration :: Maybe OAG.GeneratorConfiguration
  }
  deriving (Show, Eq)

-- | The 'Generator' monad is used to pass a 'MR.Reader' environment to functions in need of resolving references
-- and collects log messages.
newtype Generator a = Generator {unGenerator :: MW.WriterT OAL.LogEntries (MR.Reader GeneratorEnvironment) a}
  deriving (Functor, Applicative, Monad, MR.MonadReader GeneratorEnvironment, MW.MonadWriter OAL.LogEntries)

-- | Runs the generator monad within a provided environment.
runGenerator :: GeneratorEnvironment -> Generator a -> (a, OAL.LogEntries)
runGenerator e (Generator g) = MR.runReader (MW.runWriterT g) e

-- | Create an environment based on a 'Ref.ReferenceMap' and 'OAO.Flags'
createEnvironment :: OAO.Options -> Maybe OAG.GeneratorConfiguration -> Ref.ReferenceMap -> GeneratorEnvironment
createEnvironment options generatorConfiguration references =
  GeneratorEnvironment
    { currentPath = [],
      references = references,
      options = options,
      generatorConfiguration = generatorConfiguration
    }

-- | Writes a log message to a 'Generator' monad
logMessage :: OAL.LogSeverity -> Text -> Generator ()
logMessage logEntrySeverity logEntryMessage = do
  logEntryPath <- getCurrentPath
  MW.tell [OAL.LogEntry {..}]

-- | Writes an error to a 'Generator' monad
logError :: Text -> Generator ()
logError = logMessage OAL.ErrorSeverity

-- | Writes a warning to a 'Generator' monad
logWarning :: Text -> Generator ()
logWarning = logMessage OAL.WarningSeverity

-- | Writes an info to a 'Generator' monad
logInfo :: Text -> Generator ()
logInfo = logMessage OAL.InfoSeverity

-- | Writes a trace to a 'Generator' monad
logTrace :: Text -> Generator ()
logTrace = logMessage OAL.TraceSeverity

-- | This function can be used to tell the 'Generator' monad where in the OpenAPI specification the generator currently is
nested :: Text -> Generator a -> Generator a
nested pathItem = MR.local $ \g -> g {currentPath = currentPath g <> [pathItem]}

-- | This function can be used to tell the 'Generator' monad where in the OpenAPI specification the generator currently is (ignoring any previous path changes)
resetPath :: [Text] -> Generator a -> Generator a
resetPath path = MR.local $ \g -> g {currentPath = path}

getCurrentPath :: Generator [Text]
getCurrentPath = MR.asks currentPath

getFromGeneratorConfiguration :: (OAG.GeneratorConfiguration -> a) -> Generator (Maybe a)
getFromGeneratorConfiguration f = MR.asks (fmap f . generatorConfiguration)

appendToPath :: [Text] -> Generator [Text]
appendToPath path = do
  p <- getCurrentPath
  pure $ p <> path

-- | Helper function to create a lookup function for a specific type
createReferenceLookupM :: (Text -> Ref.ReferenceMap -> Maybe a) -> Text -> Generator (Maybe a)
createReferenceLookupM fn key = MR.asks $ fn key . references

-- | Resolve a 'OAS.SchemaObject' reference from within the 'Generator' monad
getSchemaReferenceM :: Text -> Generator (Maybe OAS.SchemaObject)
getSchemaReferenceM = createReferenceLookupM Ref.getSchemaReference

-- | Resolve a 'OAT.ResponseObject' reference from within the 'Generator' monad
getResponseReferenceM :: Text -> Generator (Maybe OAT.ResponseObject)
getResponseReferenceM = createReferenceLookupM Ref.getResponseReference

-- | Resolve a 'OAT.ParameterObject' reference from within the 'Generator' monad
getParameterReferenceM :: Text -> Generator (Maybe OAT.ParameterObject)
getParameterReferenceM = createReferenceLookupM Ref.getParameterReference

-- | Resolve a 'OAT.ExampleObject' reference from within the 'Generator' monad
getExampleReferenceM :: Text -> Generator (Maybe OAT.ExampleObject)
getExampleReferenceM = createReferenceLookupM Ref.getExampleReference

-- | Resolve a 'OAT.RequestBodyObject' reference from within the 'Generator' monad
getRequestBodyReferenceM :: Text -> Generator (Maybe OAT.RequestBodyObject)
getRequestBodyReferenceM = createReferenceLookupM Ref.getRequestBodyReference

-- | Resolve a 'OAT.HeaderObject' reference from within the 'Generator' monad
getHeaderReferenceM :: Text -> Generator (Maybe OAT.HeaderObject)
getHeaderReferenceM = createReferenceLookupM Ref.getHeaderReference

-- | Resolve a 'OAT.SecuritySchemeObject' reference from within the 'Generator' monad
getSecuritySchemeReferenceM :: Text -> Generator (Maybe OAT.SecuritySchemeObject)
getSecuritySchemeReferenceM = createReferenceLookupM Ref.getSecuritySchemeReference

-- | Get all flags passed to the program
getFlags :: Generator OAO.Flags
getFlags = MR.asks $ OAO.optFlags . options

-- | Get a specific flag selected by @f@
getFlag :: (OAO.Flags -> a) -> Generator a
getFlag f = MR.asks $ f . OAO.optFlags . options
