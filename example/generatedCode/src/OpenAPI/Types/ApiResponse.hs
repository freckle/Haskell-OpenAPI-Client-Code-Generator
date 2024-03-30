-- CHANGE WITH CAUTION: This is a generated code file generated by https://github.com/Haskell-OpenAPI-Code-Generator/Haskell-OpenAPI-Client-Code-Generator.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

-- | Contains the types generated from the schema ApiResponse
module OpenAPI.Types.ApiResponse where

import qualified Prelude as GHC.Integer.Type
import qualified Prelude as GHC.Maybe
import qualified Control.Monad.Fail
import qualified Data.Aeson
import qualified Data.Aeson as Data.Aeson.Encoding.Internal
import qualified Data.Aeson as Data.Aeson.Types
import qualified Data.Aeson as Data.Aeson.Types.FromJSON
import qualified Data.Aeson as Data.Aeson.Types.ToJSON
import qualified Data.Aeson as Data.Aeson.Types.Internal
import qualified Data.ByteString
import qualified Data.ByteString as Data.ByteString.Internal
import qualified Data.Foldable
import qualified Data.Functor
import qualified Data.Maybe
import qualified Data.Scientific
import qualified Data.Text
import qualified Data.Text as Data.Text.Internal
import qualified Data.Time.Calendar as Data.Time.Calendar.Days
import qualified Data.Time.LocalTime as Data.Time.LocalTime.Internal.ZonedTime
import qualified GHC.Base
import qualified GHC.Classes
import qualified GHC.Int
import qualified GHC.Show
import qualified GHC.Types
import qualified OpenAPI.Common
import OpenAPI.TypeAlias

-- | Defines the object schema located at @components.schemas.ApiResponse@ in the specification.
-- 
-- 
data ApiResponse = ApiResponse {
  -- | code
  apiResponseCode :: (GHC.Maybe.Maybe GHC.Int.Int32)
  -- | message
  , apiResponseMessage :: (GHC.Maybe.Maybe Data.Text.Internal.Text)
  -- | type
  , apiResponseType :: (GHC.Maybe.Maybe Data.Text.Internal.Text)
  } deriving (GHC.Show.Show
  , GHC.Classes.Eq)
instance Data.Aeson.Types.ToJSON.ToJSON ApiResponse
    where {toJSON obj = Data.Aeson.Types.Internal.object (Data.Foldable.concat (Data.Maybe.maybe GHC.Base.mempty (GHC.Base.pure GHC.Base.. ("code" Data.Aeson.Types.ToJSON..=)) (apiResponseCode obj) : Data.Maybe.maybe GHC.Base.mempty (GHC.Base.pure GHC.Base.. ("message" Data.Aeson.Types.ToJSON..=)) (apiResponseMessage obj) : Data.Maybe.maybe GHC.Base.mempty (GHC.Base.pure GHC.Base.. ("type" Data.Aeson.Types.ToJSON..=)) (apiResponseType obj) : GHC.Base.mempty));
           toEncoding obj = Data.Aeson.Encoding.Internal.pairs (GHC.Base.mconcat (Data.Foldable.concat (Data.Maybe.maybe GHC.Base.mempty (GHC.Base.pure GHC.Base.. ("code" Data.Aeson.Types.ToJSON..=)) (apiResponseCode obj) : Data.Maybe.maybe GHC.Base.mempty (GHC.Base.pure GHC.Base.. ("message" Data.Aeson.Types.ToJSON..=)) (apiResponseMessage obj) : Data.Maybe.maybe GHC.Base.mempty (GHC.Base.pure GHC.Base.. ("type" Data.Aeson.Types.ToJSON..=)) (apiResponseType obj) : GHC.Base.mempty)))}
instance Data.Aeson.Types.FromJSON.FromJSON ApiResponse
    where {parseJSON = Data.Aeson.Types.FromJSON.withObject "ApiResponse" (\obj -> ((GHC.Base.pure ApiResponse GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:! "code")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:! "message")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:! "type"))}
-- | Create a new 'ApiResponse' with all required fields.
mkApiResponse :: ApiResponse
mkApiResponse = ApiResponse{apiResponseCode = GHC.Maybe.Nothing,
                            apiResponseMessage = GHC.Maybe.Nothing,
                            apiResponseType = GHC.Maybe.Nothing}
