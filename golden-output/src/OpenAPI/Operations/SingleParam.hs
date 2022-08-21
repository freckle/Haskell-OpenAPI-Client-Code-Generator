-- CHANGE WITH CAUTION: This is a generated code file generated by https://github.com/Haskell-OpenAPI-Code-Generator/Haskell-OpenAPI-Client-Code-Generator.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE MultiWayIf #-}

-- | Contains the different functions to run the operation singleParam
module OpenAPI.Operations.SingleParam where

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
import qualified Data.ByteString.Char8
import qualified Data.ByteString.Char8 as Data.ByteString.Internal
import qualified Data.Either
import qualified Data.Foldable
import qualified Data.Functor
import qualified Data.Maybe
import qualified Data.Scientific
import qualified Data.Text
import qualified Data.Text.Internal
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

-- | > GET /pet/singleparam
-- 
-- Operation with a single parameter
singleParam :: forall m . OpenAPI.Common.MonadHTTP m => SingleParamParametersStatus -- ^ status: Status values that need to be considered for filter
  -> OpenAPI.Common.ClientT m (Network.HTTP.Client.Types.Response SingleParamResponse) -- ^ Monadic computation which returns the result of the operation
singleParam status = GHC.Base.fmap (\response_0 -> GHC.Base.fmap (Data.Either.either SingleParamResponseError GHC.Base.id GHC.Base.. (\response body -> if | (\status_1 -> Network.HTTP.Types.Status.statusCode status_1 GHC.Classes.== 200) (Network.HTTP.Client.Types.responseStatus response) -> SingleParamResponse200 Data.Functor.<$> (Data.Aeson.eitherDecodeStrict body :: Data.Either.Either GHC.Base.String
                                                                                                                                                                                                                                                                                                                                                                                                      Dog)
                                                                                                                                                           | GHC.Base.otherwise -> Data.Either.Left "Missing default response type") response_0) response_0) (OpenAPI.Common.doCallWithConfigurationM (Data.Text.toUpper GHC.Base.$ Data.Text.pack "GET") (Data.Text.pack "/pet/singleparam") [OpenAPI.Common.QueryParameter (Data.Text.pack "status") (GHC.Maybe.Just GHC.Base.$ Data.Aeson.Types.ToJSON.toJSON status) (Data.Text.pack "form") GHC.Types.False])
-- | Defines the enum schema located at @paths.\/pet\/singleparam.GET.parameters.[0].schema@ in the specification.
-- 
-- Represents the parameter named \'status\'
-- 
-- Status values that need to be considered for filter
data SingleParamParametersStatus =
   SingleParamParametersStatusOther Data.Aeson.Types.Internal.Value -- ^ This case is used if the value encountered during decoding does not match any of the provided cases in the specification.
  | SingleParamParametersStatusTyped Data.Text.Internal.Text -- ^ This constructor can be used to send values to the server which are not present in the specification yet.
  | SingleParamParametersStatusEnumAvailable -- ^ Represents the JSON value @"available"@
  | SingleParamParametersStatusEnumPending -- ^ Represents the JSON value @"pending"@
  | SingleParamParametersStatusEnumSold -- ^ Represents the JSON value @"sold"@
  deriving (GHC.Show.Show, GHC.Classes.Eq)
instance Data.Aeson.Types.ToJSON.ToJSON SingleParamParametersStatus
    where toJSON (SingleParamParametersStatusOther val) = val
          toJSON (SingleParamParametersStatusTyped val) = Data.Aeson.Types.ToJSON.toJSON val
          toJSON (SingleParamParametersStatusEnumAvailable) = "available"
          toJSON (SingleParamParametersStatusEnumPending) = "pending"
          toJSON (SingleParamParametersStatusEnumSold) = "sold"
instance Data.Aeson.Types.FromJSON.FromJSON SingleParamParametersStatus
    where parseJSON val = GHC.Base.pure (if | val GHC.Classes.== "available" -> SingleParamParametersStatusEnumAvailable
                                            | val GHC.Classes.== "pending" -> SingleParamParametersStatusEnumPending
                                            | val GHC.Classes.== "sold" -> SingleParamParametersStatusEnumSold
                                            | GHC.Base.otherwise -> SingleParamParametersStatusOther val)
-- | Represents a response of the operation 'singleParam'.
-- 
-- The response constructor is chosen by the status code of the response. If no case matches (no specific case for the response code, no range case, no default case), 'SingleParamResponseError' is used.
data SingleParamResponse =
   SingleParamResponseError GHC.Base.String -- ^ Means either no matching case available or a parse error
  | SingleParamResponse200 Dog -- ^ successful operation
  deriving (GHC.Show.Show, GHC.Classes.Eq)
-- | > GET /pet/singleparam
-- 
-- The same as 'singleParam' but accepts an explicit configuration.
singleParamWithConfiguration :: forall m . OpenAPI.Common.MonadHTTP m => OpenAPI.Common.Configuration -- ^ The configuration to use in the request
  -> SingleParamParametersStatus -- ^ status: Status values that need to be considered for filter
  -> m (Network.HTTP.Client.Types.Response SingleParamResponse) -- ^ Monadic computation which returns the result of the operation
singleParamWithConfiguration config
                             status = GHC.Base.fmap (\response_2 -> GHC.Base.fmap (Data.Either.either SingleParamResponseError GHC.Base.id GHC.Base.. (\response body -> if | (\status_3 -> Network.HTTP.Types.Status.statusCode status_3 GHC.Classes.== 200) (Network.HTTP.Client.Types.responseStatus response) -> SingleParamResponse200 Data.Functor.<$> (Data.Aeson.eitherDecodeStrict body :: Data.Either.Either GHC.Base.String
                                                                                                                                                                                                                                                                                                                                                                                                                       Dog)
                                                                                                                                                                            | GHC.Base.otherwise -> Data.Either.Left "Missing default response type") response_2) response_2) (OpenAPI.Common.doCallWithConfiguration config (Data.Text.toUpper GHC.Base.$ Data.Text.pack "GET") (Data.Text.pack "/pet/singleparam") [OpenAPI.Common.QueryParameter (Data.Text.pack "status") (GHC.Maybe.Just GHC.Base.$ Data.Aeson.Types.ToJSON.toJSON status) (Data.Text.pack "form") GHC.Types.False])
-- | > GET /pet/singleparam
-- 
-- The same as 'singleParam' but returns the raw 'Data.ByteString.Char8.ByteString'.
singleParamRaw :: forall m . OpenAPI.Common.MonadHTTP m => SingleParamParametersStatus -- ^ status: Status values that need to be considered for filter
  -> OpenAPI.Common.ClientT m (Network.HTTP.Client.Types.Response Data.ByteString.Internal.ByteString) -- ^ Monadic computation which returns the result of the operation
singleParamRaw status = GHC.Base.id (OpenAPI.Common.doCallWithConfigurationM (Data.Text.toUpper GHC.Base.$ Data.Text.pack "GET") (Data.Text.pack "/pet/singleparam") [OpenAPI.Common.QueryParameter (Data.Text.pack "status") (GHC.Maybe.Just GHC.Base.$ Data.Aeson.Types.ToJSON.toJSON status) (Data.Text.pack "form") GHC.Types.False])
-- | > GET /pet/singleparam
-- 
-- The same as 'singleParam' but accepts an explicit configuration and returns the raw 'Data.ByteString.Char8.ByteString'.
singleParamWithConfigurationRaw :: forall m . OpenAPI.Common.MonadHTTP m => OpenAPI.Common.Configuration -- ^ The configuration to use in the request
  -> SingleParamParametersStatus -- ^ status: Status values that need to be considered for filter
  -> m (Network.HTTP.Client.Types.Response Data.ByteString.Internal.ByteString) -- ^ Monadic computation which returns the result of the operation
singleParamWithConfigurationRaw config
                                status = GHC.Base.id (OpenAPI.Common.doCallWithConfiguration config (Data.Text.toUpper GHC.Base.$ Data.Text.pack "GET") (Data.Text.pack "/pet/singleparam") [OpenAPI.Common.QueryParameter (Data.Text.pack "status") (GHC.Maybe.Just GHC.Base.$ Data.Aeson.Types.ToJSON.toJSON status) (Data.Text.pack "form") GHC.Types.False])
