-- CHANGE WITH CAUTION: This is a generated code file generated by https://github.com/Haskell-OpenAPI-Code-Generator/Haskell-OpenAPI-Client-Code-Generator.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE MultiWayIf #-}

-- | Contains the different functions to run the operation getUserByName
module OpenAPI.Operations.GetUserByName where

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

-- | > GET /user/{username}
-- 
-- 
getUserByName :: forall m . OpenAPI.Common.MonadHTTP m => Data.Text.Internal.Text -- ^ username: The name that needs to be fetched. Use user1 for testing. 
  -> OpenAPI.Common.ClientT m (Network.HTTP.Client.Types.Response GetUserByNameResponse) -- ^ Monadic computation which returns the result of the operation
getUserByName username = GHC.Base.fmap (\response_0 -> GHC.Base.fmap (Data.Either.either GetUserByNameResponseError GHC.Base.id GHC.Base.. (\response body -> if | (\status_1 -> Network.HTTP.Types.Status.statusCode status_1 GHC.Classes.== 200) (Network.HTTP.Client.Types.responseStatus response) -> GetUserByNameResponse200 Data.Functor.<$> (Data.Aeson.eitherDecodeStrict body :: Data.Either.Either GHC.Base.String
                                                                                                                                                                                                                                                                                                                                                                                                              User)
                                                                                                                                                                 | (\status_2 -> Network.HTTP.Types.Status.statusCode status_2 GHC.Classes.== 400) (Network.HTTP.Client.Types.responseStatus response) -> Data.Either.Right GetUserByNameResponse400
                                                                                                                                                                 | (\status_3 -> Network.HTTP.Types.Status.statusCode status_3 GHC.Classes.== 404) (Network.HTTP.Client.Types.responseStatus response) -> Data.Either.Right GetUserByNameResponse404
                                                                                                                                                                 | GHC.Base.otherwise -> Data.Either.Left "Missing default response type") response_0) response_0) (OpenAPI.Common.doCallWithConfigurationM (Data.Text.toUpper GHC.Base.$ Data.Text.Internal.pack "GET") ("/user/" GHC.Base.<> (OpenAPI.Common.byteToText (Network.HTTP.Types.URI.urlEncode GHC.Types.True GHC.Base.$ (OpenAPI.Common.textToByte GHC.Base.$ OpenAPI.Common.stringifyModel username)) GHC.Base.<> "")) GHC.Base.mempty)
-- | Represents a response of the operation 'getUserByName'.
-- 
-- The response constructor is chosen by the status code of the response. If no case matches (no specific case for the response code, no range case, no default case), 'GetUserByNameResponseError' is used.
data GetUserByNameResponse =
   GetUserByNameResponseError GHC.Base.String -- ^ Means either no matching case available or a parse error
  | GetUserByNameResponse200 User -- ^ successful operation
  | GetUserByNameResponse400 -- ^ Invalid username supplied
  | GetUserByNameResponse404 -- ^ User not found
  deriving (GHC.Show.Show, GHC.Classes.Eq)
-- | > GET /user/{username}
-- 
-- The same as 'getUserByName' but accepts an explicit configuration.
getUserByNameWithConfiguration :: forall m . OpenAPI.Common.MonadHTTP m => OpenAPI.Common.Configuration -- ^ The configuration to use in the request
  -> Data.Text.Internal.Text -- ^ username: The name that needs to be fetched. Use user1 for testing. 
  -> m (Network.HTTP.Client.Types.Response GetUserByNameResponse) -- ^ Monadic computation which returns the result of the operation
getUserByNameWithConfiguration config
                               username = GHC.Base.fmap (\response_4 -> GHC.Base.fmap (Data.Either.either GetUserByNameResponseError GHC.Base.id GHC.Base.. (\response body -> if | (\status_5 -> Network.HTTP.Types.Status.statusCode status_5 GHC.Classes.== 200) (Network.HTTP.Client.Types.responseStatus response) -> GetUserByNameResponse200 Data.Functor.<$> (Data.Aeson.eitherDecodeStrict body :: Data.Either.Either GHC.Base.String
                                                                                                                                                                                                                                                                                                                                                                                                                               User)
                                                                                                                                                                                  | (\status_6 -> Network.HTTP.Types.Status.statusCode status_6 GHC.Classes.== 400) (Network.HTTP.Client.Types.responseStatus response) -> Data.Either.Right GetUserByNameResponse400
                                                                                                                                                                                  | (\status_7 -> Network.HTTP.Types.Status.statusCode status_7 GHC.Classes.== 404) (Network.HTTP.Client.Types.responseStatus response) -> Data.Either.Right GetUserByNameResponse404
                                                                                                                                                                                  | GHC.Base.otherwise -> Data.Either.Left "Missing default response type") response_4) response_4) (OpenAPI.Common.doCallWithConfiguration config (Data.Text.toUpper GHC.Base.$ Data.Text.Internal.pack "GET") ("/user/" GHC.Base.<> (OpenAPI.Common.byteToText (Network.HTTP.Types.URI.urlEncode GHC.Types.True GHC.Base.$ (OpenAPI.Common.textToByte GHC.Base.$ OpenAPI.Common.stringifyModel username)) GHC.Base.<> "")) GHC.Base.mempty)
-- | > GET /user/{username}
-- 
-- The same as 'getUserByName' but returns the raw 'Data.ByteString.ByteString'.
getUserByNameRaw :: forall m . OpenAPI.Common.MonadHTTP m => Data.Text.Internal.Text -- ^ username: The name that needs to be fetched. Use user1 for testing. 
  -> OpenAPI.Common.ClientT m (Network.HTTP.Client.Types.Response Data.ByteString.Internal.Type.ByteString) -- ^ Monadic computation which returns the result of the operation
getUserByNameRaw username = GHC.Base.id (OpenAPI.Common.doCallWithConfigurationM (Data.Text.toUpper GHC.Base.$ Data.Text.Internal.pack "GET") ("/user/" GHC.Base.<> (OpenAPI.Common.byteToText (Network.HTTP.Types.URI.urlEncode GHC.Types.True GHC.Base.$ (OpenAPI.Common.textToByte GHC.Base.$ OpenAPI.Common.stringifyModel username)) GHC.Base.<> "")) GHC.Base.mempty)
-- | > GET /user/{username}
-- 
-- The same as 'getUserByName' but accepts an explicit configuration and returns the raw 'Data.ByteString.ByteString'.
getUserByNameWithConfigurationRaw :: forall m . OpenAPI.Common.MonadHTTP m => OpenAPI.Common.Configuration -- ^ The configuration to use in the request
  -> Data.Text.Internal.Text -- ^ username: The name that needs to be fetched. Use user1 for testing. 
  -> m (Network.HTTP.Client.Types.Response Data.ByteString.Internal.Type.ByteString) -- ^ Monadic computation which returns the result of the operation
getUserByNameWithConfigurationRaw config
                                  username = GHC.Base.id (OpenAPI.Common.doCallWithConfiguration config (Data.Text.toUpper GHC.Base.$ Data.Text.Internal.pack "GET") ("/user/" GHC.Base.<> (OpenAPI.Common.byteToText (Network.HTTP.Types.URI.urlEncode GHC.Types.True GHC.Base.$ (OpenAPI.Common.textToByte GHC.Base.$ OpenAPI.Common.stringifyModel username)) GHC.Base.<> "")) GHC.Base.mempty)
