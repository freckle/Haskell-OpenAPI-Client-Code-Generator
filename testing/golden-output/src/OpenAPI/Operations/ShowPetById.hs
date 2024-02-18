-- CHANGE WITH CAUTION: This is a generated code file generated by https://github.com/Haskell-OpenAPI-Code-Generator/Haskell-OpenAPI-Client-Code-Generator.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE MultiWayIf #-}

-- | Contains the different functions to run the operation showPetById
module OpenAPI.Operations.ShowPetById where

import qualified Prelude as GHC.Integer.Type
import qualified Prelude as GHC.Maybe
import qualified Control.Monad.Fail
import qualified Control.Monad.Trans.Reader
import qualified Data.Aeson
import qualified Data.Aeson as Data.Aeson.Encoding.Internal
import qualified Data.Aeson as Data.Aeson.Types
import qualified Data.Aeson as Data.Aeson.Types.FromJSON
import qualified Data.Aeson as Data.Aeson.Types.ToJSON
import qualified Data.Aeson as Data.Aeson.Types.Internal
import qualified Data.ByteString
import qualified Data.ByteString as Data.ByteString.Internal
import qualified Data.ByteString as Data.ByteString.Internal.Type
import qualified Data.Either
import qualified Data.Foldable
import qualified Data.Functor
import qualified Data.Maybe
import qualified Data.Scientific
import qualified Data.Text
import qualified Data.Text as Data.Text.Internal
import qualified Data.Time.Calendar as Data.Time.Calendar.Days
import qualified Data.Time.LocalTime as Data.Time.LocalTime.Internal.ZonedTime
import qualified Data.Vector
import qualified GHC.Base
import qualified GHC.Classes
import qualified GHC.Int
import qualified GHC.Show
import qualified GHC.Types
import qualified Network.HTTP.Client
import qualified Network.HTTP.Client as Network.HTTP.Client.Request
import qualified Network.HTTP.Client as Network.HTTP.Client.Types
import qualified Network.HTTP.Simple
import qualified Network.HTTP.Types
import qualified Network.HTTP.Types as Network.HTTP.Types.Status
import qualified Network.HTTP.Types as Network.HTTP.Types.URI
import qualified OpenAPI.Common
import OpenAPI.Types

-- | > GET /pets/{petId}
-- 
-- Info for a specific pet
showPetById :: forall m . OpenAPI.Common.MonadHTTP m => Data.Text.Internal.Text -- ^ petId: The id of the pet to retrieve
  -> OpenAPI.Common.ClientT m (Network.HTTP.Client.Types.Response ShowPetByIdResponse) -- ^ Monadic computation which returns the result of the operation
showPetById petId = GHC.Base.fmap (\response_0 -> GHC.Base.fmap (Data.Either.either ShowPetByIdResponseError GHC.Base.id GHC.Base.. (\response body -> if | GHC.Base.const GHC.Types.True (Network.HTTP.Client.Types.responseStatus response) -> ShowPetByIdResponseDefault Data.Functor.<$> (Data.Aeson.eitherDecodeStrict body :: Data.Either.Either GHC.Base.String
                                                                                                                                                                                                                                                                                                                                                       Dog)
                                                                                                                                                          | GHC.Base.otherwise -> Data.Either.Left "Missing default response type") response_0) response_0) (OpenAPI.Common.doCallWithConfigurationM (Data.Text.toUpper GHC.Base.$ Data.Text.Internal.pack "GET") ("/pets/" GHC.Base.<> (OpenAPI.Common.byteToText (Network.HTTP.Types.URI.urlEncode GHC.Types.True GHC.Base.$ (OpenAPI.Common.textToByte GHC.Base.$ OpenAPI.Common.stringifyModel petId)) GHC.Base.<> "")) GHC.Base.mempty)
-- | Represents a response of the operation 'showPetById'.
-- 
-- The response constructor is chosen by the status code of the response. If no case matches (no specific case for the response code, no range case, no default case), 'ShowPetByIdResponseError' is used.
data ShowPetByIdResponse =
   ShowPetByIdResponseError GHC.Base.String -- ^ Means either no matching case available or a parse error
  | ShowPetByIdResponseDefault Dog -- ^ Expected response to a valid request
  deriving (GHC.Show.Show, GHC.Classes.Eq)
-- | > GET /pets/{petId}
-- 
-- The same as 'showPetById' but accepts an explicit configuration.
showPetByIdWithConfiguration :: forall m . OpenAPI.Common.MonadHTTP m => OpenAPI.Common.Configuration -- ^ The configuration to use in the request
  -> Data.Text.Internal.Text -- ^ petId: The id of the pet to retrieve
  -> m (Network.HTTP.Client.Types.Response ShowPetByIdResponse) -- ^ Monadic computation which returns the result of the operation
showPetByIdWithConfiguration config
                             petId = GHC.Base.fmap (\response_1 -> GHC.Base.fmap (Data.Either.either ShowPetByIdResponseError GHC.Base.id GHC.Base.. (\response body -> if | GHC.Base.const GHC.Types.True (Network.HTTP.Client.Types.responseStatus response) -> ShowPetByIdResponseDefault Data.Functor.<$> (Data.Aeson.eitherDecodeStrict body :: Data.Either.Either GHC.Base.String
                                                                                                                                                                                                                                                                                                                                                                        Dog)
                                                                                                                                                                           | GHC.Base.otherwise -> Data.Either.Left "Missing default response type") response_1) response_1) (OpenAPI.Common.doCallWithConfiguration config (Data.Text.toUpper GHC.Base.$ Data.Text.Internal.pack "GET") ("/pets/" GHC.Base.<> (OpenAPI.Common.byteToText (Network.HTTP.Types.URI.urlEncode GHC.Types.True GHC.Base.$ (OpenAPI.Common.textToByte GHC.Base.$ OpenAPI.Common.stringifyModel petId)) GHC.Base.<> "")) GHC.Base.mempty)
-- | > GET /pets/{petId}
-- 
-- The same as 'showPetById' but returns the raw 'Data.ByteString.ByteString'.
showPetByIdRaw :: forall m . OpenAPI.Common.MonadHTTP m => Data.Text.Internal.Text -- ^ petId: The id of the pet to retrieve
  -> OpenAPI.Common.ClientT m (Network.HTTP.Client.Types.Response Data.ByteString.Internal.Type.ByteString) -- ^ Monadic computation which returns the result of the operation
showPetByIdRaw petId = GHC.Base.id (OpenAPI.Common.doCallWithConfigurationM (Data.Text.toUpper GHC.Base.$ Data.Text.Internal.pack "GET") ("/pets/" GHC.Base.<> (OpenAPI.Common.byteToText (Network.HTTP.Types.URI.urlEncode GHC.Types.True GHC.Base.$ (OpenAPI.Common.textToByte GHC.Base.$ OpenAPI.Common.stringifyModel petId)) GHC.Base.<> "")) GHC.Base.mempty)
-- | > GET /pets/{petId}
-- 
-- The same as 'showPetById' but accepts an explicit configuration and returns the raw 'Data.ByteString.ByteString'.
showPetByIdWithConfigurationRaw :: forall m . OpenAPI.Common.MonadHTTP m => OpenAPI.Common.Configuration -- ^ The configuration to use in the request
  -> Data.Text.Internal.Text -- ^ petId: The id of the pet to retrieve
  -> m (Network.HTTP.Client.Types.Response Data.ByteString.Internal.Type.ByteString) -- ^ Monadic computation which returns the result of the operation
showPetByIdWithConfigurationRaw config
                                petId = GHC.Base.id (OpenAPI.Common.doCallWithConfiguration config (Data.Text.toUpper GHC.Base.$ Data.Text.Internal.pack "GET") ("/pets/" GHC.Base.<> (OpenAPI.Common.byteToText (Network.HTTP.Types.URI.urlEncode GHC.Types.True GHC.Base.$ (OpenAPI.Common.textToByte GHC.Base.$ OpenAPI.Common.stringifyModel petId)) GHC.Base.<> "")) GHC.Base.mempty)
