{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Contains the types generated from the schema PetByType
module OpenAPI.Types.PetByType where

import qualified Prelude as GHC.Integer.Type
import qualified Prelude as GHC.Maybe
import qualified Data.Aeson
import qualified Data.Aeson as Data.Aeson.Types
import qualified Data.Aeson as Data.Aeson.Types.FromJSON
import qualified Data.Aeson as Data.Aeson.Types.ToJSON
import qualified Data.Aeson as Data.Aeson.Types.Internal
import qualified Data.ByteString.Char8
import qualified Data.ByteString.Char8 as Data.ByteString.Internal
import qualified Data.Functor
import qualified Data.Scientific
import qualified Data.Text
import qualified Data.Text.Internal
import qualified Data.Time.Calendar as Data.Time.Calendar.Days
import qualified Data.Time.LocalTime as Data.Time.LocalTime.Internal.ZonedTime
import qualified GHC.Base
import qualified GHC.Classes
import qualified GHC.Generics
import qualified GHC.Int
import qualified GHC.Show
import qualified GHC.Types
import qualified OpenAPI.Common

-- | Defines the data type for the schema PetByType
-- 
-- 
data PetByType = PetByType {
  -- | hunts
  petByTypeHunts :: (GHC.Maybe.Maybe GHC.Types.Bool)
  -- | pet_type
  , petByTypePet_type :: PetByTypePet_type
  } deriving (GHC.Show.Show
  , GHC.Classes.Eq)
instance Data.Aeson.ToJSON PetByType
    where toJSON obj = Data.Aeson.object ((Data.Aeson..=) "hunts" (petByTypeHunts obj) : (Data.Aeson..=) "pet_type" (petByTypePet_type obj) : [])
          toEncoding obj = Data.Aeson.pairs ((Data.Aeson..=) "hunts" (petByTypeHunts obj) GHC.Base.<> (Data.Aeson..=) "pet_type" (petByTypePet_type obj))
instance Data.Aeson.Types.FromJSON.FromJSON PetByType
    where parseJSON = Data.Aeson.Types.FromJSON.withObject "PetByType" (\obj -> (GHC.Base.pure PetByType GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:? "hunts")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..: "pet_type"))
-- | Defines the enum schema PetByTypePet_type
-- 
-- 
data PetByTypePet_type
    = PetByTypePet_typeEnumOther Data.Aeson.Types.Internal.Value
    | PetByTypePet_typeEnumTyped GHC.Base.String
    | PetByTypePet_typeEnumString_Cat
    | PetByTypePet_typeEnumString_Dog
    deriving (GHC.Show.Show, GHC.Classes.Eq)
instance Data.Aeson.ToJSON PetByTypePet_type
    where toJSON (PetByTypePet_typeEnumOther patternName) = Data.Aeson.Types.ToJSON.toJSON patternName
          toJSON (PetByTypePet_typeEnumTyped patternName) = Data.Aeson.Types.ToJSON.toJSON patternName
          toJSON (PetByTypePet_typeEnumString_Cat) = Data.Aeson.Types.Internal.String GHC.Base.$ Data.Text.pack "Cat"
          toJSON (PetByTypePet_typeEnumString_Dog) = Data.Aeson.Types.Internal.String GHC.Base.$ Data.Text.pack "Dog"
instance Data.Aeson.FromJSON PetByTypePet_type
    where parseJSON val = GHC.Base.pure (if val GHC.Classes.== (Data.Aeson.Types.Internal.String GHC.Base.$ Data.Text.pack "Cat")
                                          then PetByTypePet_typeEnumString_Cat
                                          else if val GHC.Classes.== (Data.Aeson.Types.Internal.String GHC.Base.$ Data.Text.pack "Dog")
                                                then PetByTypePet_typeEnumString_Dog
                                                else PetByTypePet_typeEnumOther val)