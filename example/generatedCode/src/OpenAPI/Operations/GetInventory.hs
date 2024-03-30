-- CHANGE WITH CAUTION: This is a generated code file generated by https://github.com/Haskell-OpenAPI-Code-Generator/Haskell-OpenAPI-Client-Code-Generator.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE MultiWayIf #-}

-- | Contains the different functions to run the operation getInventory
module OpenAPI.Operations.GetInventory where

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

-- | > GET /store/inventory
-- 
-- Returns a map of status codes to quantities
getInventory :: forall m . OpenAPI.Common.MonadHTTP m => OpenAPI.Common.ClientT m (Network.HTTP.Client.Types.Response GetInventoryResponse) -- ^ Monadic computation which returns the result of the operation
getInventory = GHC.Base.fmap (\response_0 -> GHC.Base.fmap (Data.Either.either GetInventoryResponseError GHC.Base.id GHC.Base.. (\response body -> if | (\status_1 -> Network.HTTP.Types.Status.statusCode status_1 GHC.Classes.== 200) (Network.HTTP.Client.Types.responseStatus response) -> GetInventoryResponse200 Data.Functor.<$> (Data.Aeson.eitherDecodeStrict body :: Data.Either.Either GHC.Base.String
                                                                                                                                                                                                                                                                                                                                                                                                  Data.Aeson.Types.Internal.Object)
                                                                                                                                                      | GHC.Base.otherwise -> Data.Either.Left "Missing default response type") response_0) response_0) (OpenAPI.Common.doCallWithConfigurationM (Data.Text.toUpper GHC.Base.$ Data.Text.Internal.pack "GET") "/store/inventory" GHC.Base.mempty)
-- | Represents a response of the operation 'getInventory'.
-- 
-- The response constructor is chosen by the status code of the response. If no case matches (no specific case for the response code, no range case, no default case), 'GetInventoryResponseError' is used.
data GetInventoryResponse =
   GetInventoryResponseError GHC.Base.String -- ^ Means either no matching case available or a parse error
  | GetInventoryResponse200 Data.Aeson.Types.Internal.Object -- ^ successful operation
  deriving (GHC.Show.Show, GHC.Classes.Eq)
-- | > GET /store/inventory
-- 
-- The same as 'getInventory' but accepts an explicit configuration.
getInventoryWithConfiguration :: forall m . OpenAPI.Common.MonadHTTP m => OpenAPI.Common.Configuration -- ^ The configuration to use in the request
  -> m (Network.HTTP.Client.Types.Response GetInventoryResponse) -- ^ Monadic computation which returns the result of the operation
getInventoryWithConfiguration config = GHC.Base.fmap (\response_2 -> GHC.Base.fmap (Data.Either.either GetInventoryResponseError GHC.Base.id GHC.Base.. (\response body -> if | (\status_3 -> Network.HTTP.Types.Status.statusCode status_3 GHC.Classes.== 200) (Network.HTTP.Client.Types.responseStatus response) -> GetInventoryResponse200 Data.Functor.<$> (Data.Aeson.eitherDecodeStrict body :: Data.Either.Either GHC.Base.String
                                                                                                                                                                                                                                                                                                                                                                                                                          Data.Aeson.Types.Internal.Object)
                                                                                                                                                                              | GHC.Base.otherwise -> Data.Either.Left "Missing default response type") response_2) response_2) (OpenAPI.Common.doCallWithConfiguration config (Data.Text.toUpper GHC.Base.$ Data.Text.Internal.pack "GET") "/store/inventory" GHC.Base.mempty)
-- | > GET /store/inventory
-- 
-- The same as 'getInventory' but returns the raw 'Data.ByteString.ByteString'.
getInventoryRaw :: forall m . OpenAPI.Common.MonadHTTP m => OpenAPI.Common.ClientT m (Network.HTTP.Client.Types.Response Data.ByteString.Internal.Type.ByteString) -- ^ Monadic computation which returns the result of the operation
getInventoryRaw = GHC.Base.id (OpenAPI.Common.doCallWithConfigurationM (Data.Text.toUpper GHC.Base.$ Data.Text.Internal.pack "GET") "/store/inventory" GHC.Base.mempty)
-- | > GET /store/inventory
-- 
-- The same as 'getInventory' but accepts an explicit configuration and returns the raw 'Data.ByteString.ByteString'.
getInventoryWithConfigurationRaw :: forall m . OpenAPI.Common.MonadHTTP m => OpenAPI.Common.Configuration -- ^ The configuration to use in the request
  -> m (Network.HTTP.Client.Types.Response Data.ByteString.Internal.Type.ByteString) -- ^ Monadic computation which returns the result of the operation
getInventoryWithConfigurationRaw config = GHC.Base.id (OpenAPI.Common.doCallWithConfiguration config (Data.Text.toUpper GHC.Base.$ Data.Text.Internal.pack "GET") "/store/inventory" GHC.Base.mempty)
