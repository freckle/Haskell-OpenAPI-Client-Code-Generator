-- CHANGE WITH CAUTION: This is a generated code file generated by https://github.com/Haskell-OpenAPI-Code-Generator/Haskell-OpenAPI-Client-Code-Generator.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE MultiWayIf #-}

-- | Contains the different functions to run the operation deleteOrder
module OpenAPI.Operations.DeleteOrder where

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

-- | > DELETE /store/order/{orderId}
-- 
-- For valid response try integer IDs with positive integer value. Negative or non-integer values will generate API errors
deleteOrder :: forall m . OpenAPI.Common.MonadHTTP m => GHC.Int.Int64 -- ^ orderId: ID of the order that needs to be deleted | Constraints: Minimum  of 1.0
  -> OpenAPI.Common.ClientT m (Network.HTTP.Client.Types.Response DeleteOrderResponse) -- ^ Monadic computation which returns the result of the operation
deleteOrder orderId = GHC.Base.fmap (\response_0 -> GHC.Base.fmap (Data.Either.either DeleteOrderResponseError GHC.Base.id GHC.Base.. (\response body -> if | (\status_1 -> Network.HTTP.Types.Status.statusCode status_1 GHC.Classes.== 400) (Network.HTTP.Client.Types.responseStatus response) -> Data.Either.Right DeleteOrderResponse400
                                                                                                                                                            | (\status_2 -> Network.HTTP.Types.Status.statusCode status_2 GHC.Classes.== 404) (Network.HTTP.Client.Types.responseStatus response) -> Data.Either.Right DeleteOrderResponse404
                                                                                                                                                            | GHC.Base.otherwise -> Data.Either.Left "Missing default response type") response_0) response_0) (OpenAPI.Common.doCallWithConfigurationM (Data.Text.toUpper GHC.Base.$ Data.Text.Internal.pack "DELETE") ("/store/order/" GHC.Base.<> (OpenAPI.Common.byteToText (Network.HTTP.Types.URI.urlEncode GHC.Types.True GHC.Base.$ (OpenAPI.Common.textToByte GHC.Base.$ OpenAPI.Common.stringifyModel orderId)) GHC.Base.<> "")) GHC.Base.mempty)
-- | Represents a response of the operation 'deleteOrder'.
-- 
-- The response constructor is chosen by the status code of the response. If no case matches (no specific case for the response code, no range case, no default case), 'DeleteOrderResponseError' is used.
data DeleteOrderResponse =
   DeleteOrderResponseError GHC.Base.String -- ^ Means either no matching case available or a parse error
  | DeleteOrderResponse400 -- ^ Invalid ID supplied
  | DeleteOrderResponse404 -- ^ Order not found
  deriving (GHC.Show.Show, GHC.Classes.Eq)
-- | > DELETE /store/order/{orderId}
-- 
-- The same as 'deleteOrder' but accepts an explicit configuration.
deleteOrderWithConfiguration :: forall m . OpenAPI.Common.MonadHTTP m => OpenAPI.Common.Configuration -- ^ The configuration to use in the request
  -> GHC.Int.Int64 -- ^ orderId: ID of the order that needs to be deleted | Constraints: Minimum  of 1.0
  -> m (Network.HTTP.Client.Types.Response DeleteOrderResponse) -- ^ Monadic computation which returns the result of the operation
deleteOrderWithConfiguration config
                             orderId = GHC.Base.fmap (\response_3 -> GHC.Base.fmap (Data.Either.either DeleteOrderResponseError GHC.Base.id GHC.Base.. (\response body -> if | (\status_4 -> Network.HTTP.Types.Status.statusCode status_4 GHC.Classes.== 400) (Network.HTTP.Client.Types.responseStatus response) -> Data.Either.Right DeleteOrderResponse400
                                                                                                                                                                             | (\status_5 -> Network.HTTP.Types.Status.statusCode status_5 GHC.Classes.== 404) (Network.HTTP.Client.Types.responseStatus response) -> Data.Either.Right DeleteOrderResponse404
                                                                                                                                                                             | GHC.Base.otherwise -> Data.Either.Left "Missing default response type") response_3) response_3) (OpenAPI.Common.doCallWithConfiguration config (Data.Text.toUpper GHC.Base.$ Data.Text.Internal.pack "DELETE") ("/store/order/" GHC.Base.<> (OpenAPI.Common.byteToText (Network.HTTP.Types.URI.urlEncode GHC.Types.True GHC.Base.$ (OpenAPI.Common.textToByte GHC.Base.$ OpenAPI.Common.stringifyModel orderId)) GHC.Base.<> "")) GHC.Base.mempty)
-- | > DELETE /store/order/{orderId}
-- 
-- The same as 'deleteOrder' but returns the raw 'Data.ByteString.ByteString'.
deleteOrderRaw :: forall m . OpenAPI.Common.MonadHTTP m => GHC.Int.Int64 -- ^ orderId: ID of the order that needs to be deleted | Constraints: Minimum  of 1.0
  -> OpenAPI.Common.ClientT m (Network.HTTP.Client.Types.Response Data.ByteString.Internal.Type.ByteString) -- ^ Monadic computation which returns the result of the operation
deleteOrderRaw orderId = GHC.Base.id (OpenAPI.Common.doCallWithConfigurationM (Data.Text.toUpper GHC.Base.$ Data.Text.Internal.pack "DELETE") ("/store/order/" GHC.Base.<> (OpenAPI.Common.byteToText (Network.HTTP.Types.URI.urlEncode GHC.Types.True GHC.Base.$ (OpenAPI.Common.textToByte GHC.Base.$ OpenAPI.Common.stringifyModel orderId)) GHC.Base.<> "")) GHC.Base.mempty)
-- | > DELETE /store/order/{orderId}
-- 
-- The same as 'deleteOrder' but accepts an explicit configuration and returns the raw 'Data.ByteString.ByteString'.
deleteOrderWithConfigurationRaw :: forall m . OpenAPI.Common.MonadHTTP m => OpenAPI.Common.Configuration -- ^ The configuration to use in the request
  -> GHC.Int.Int64 -- ^ orderId: ID of the order that needs to be deleted | Constraints: Minimum  of 1.0
  -> m (Network.HTTP.Client.Types.Response Data.ByteString.Internal.Type.ByteString) -- ^ Monadic computation which returns the result of the operation
deleteOrderWithConfigurationRaw config
                                orderId = GHC.Base.id (OpenAPI.Common.doCallWithConfiguration config (Data.Text.toUpper GHC.Base.$ Data.Text.Internal.pack "DELETE") ("/store/order/" GHC.Base.<> (OpenAPI.Common.byteToText (Network.HTTP.Types.URI.urlEncode GHC.Types.True GHC.Base.$ (OpenAPI.Common.textToByte GHC.Base.$ OpenAPI.Common.stringifyModel orderId)) GHC.Base.<> "")) GHC.Base.mempty)
