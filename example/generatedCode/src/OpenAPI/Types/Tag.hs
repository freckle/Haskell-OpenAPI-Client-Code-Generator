-- CHANGE WITH CAUTION: This is a generated code file generated by https://github.com/Haskell-OpenAPI-Code-Generator/Haskell-OpenAPI-Client-Code-Generator.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

-- | Contains the types generated from the schema Tag
module OpenAPI.Types.Tag where

import qualified Prelude as GHC.Integer.Type
import qualified Prelude as GHC.Maybe
import qualified Control.Monad.Fail
import qualified Data.Aeson
import qualified Data.Aeson as Data.Aeson.Encoding.Internal
import qualified Data.Aeson as Data.Aeson.Types
import qualified Data.Aeson as Data.Aeson.Types.FromJSON
import qualified Data.Aeson as Data.Aeson.Types.ToJSON
import qualified Data.Aeson as Data.Aeson.Types.Internal
import qualified Data.ByteString.Char8
import qualified Data.ByteString.Char8 as Data.ByteString.Internal
import qualified Data.Foldable
import qualified Data.Functor
import qualified Data.Maybe
import qualified Data.Scientific
import qualified Data.Text
import qualified Data.Text.Internal
import qualified Data.Time.Calendar as Data.Time.Calendar.Days
import qualified Data.Time.LocalTime as Data.Time.LocalTime.Internal.ZonedTime
import qualified GHC.Base
import qualified GHC.Classes
import qualified GHC.Int
import qualified GHC.Show
import qualified GHC.Types
import qualified OpenAPI.Common
import OpenAPI.TypeAlias

-- | Defines the object schema located at @components.schemas.Tag@ in the specification.
-- 
-- 
data Tag = Tag {
  -- | id
  tagId :: (GHC.Maybe.Maybe GHC.Int.Int64)
  -- | name
  , tagName :: (GHC.Maybe.Maybe Data.Text.Internal.Text)
  } deriving (GHC.Show.Show
  , GHC.Classes.Eq)
instance Data.Aeson.Types.ToJSON.ToJSON Tag
    where toJSON obj = Data.Aeson.Types.Internal.object (Data.Foldable.concat (Data.Maybe.maybe GHC.Base.mempty (GHC.Base.pure GHC.Base.. ("id" Data.Aeson.Types.ToJSON..=)) (tagId obj) : Data.Maybe.maybe GHC.Base.mempty (GHC.Base.pure GHC.Base.. ("name" Data.Aeson.Types.ToJSON..=)) (tagName obj) : GHC.Base.mempty))
          toEncoding obj = Data.Aeson.Encoding.Internal.pairs (GHC.Base.mconcat (Data.Foldable.concat (Data.Maybe.maybe GHC.Base.mempty (GHC.Base.pure GHC.Base.. ("id" Data.Aeson.Types.ToJSON..=)) (tagId obj) : Data.Maybe.maybe GHC.Base.mempty (GHC.Base.pure GHC.Base.. ("name" Data.Aeson.Types.ToJSON..=)) (tagName obj) : GHC.Base.mempty)))
instance Data.Aeson.Types.FromJSON.FromJSON Tag
    where parseJSON = Data.Aeson.Types.FromJSON.withObject "Tag" (\obj -> (GHC.Base.pure Tag GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:! "id")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:! "name"))
-- | Create a new 'Tag' with all required fields.
mkTag :: Tag
mkTag = Tag{tagId = GHC.Maybe.Nothing, tagName = GHC.Maybe.Nothing}
