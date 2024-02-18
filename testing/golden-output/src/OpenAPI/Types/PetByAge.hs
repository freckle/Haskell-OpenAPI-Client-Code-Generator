-- CHANGE WITH CAUTION: This is a generated code file generated by https://github.com/Haskell-OpenAPI-Code-Generator/Haskell-OpenAPI-Client-Code-Generator.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

-- | Contains the types generated from the schema PetByAge
module OpenAPI.Types.PetByAge where

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
import {-# SOURCE #-} OpenAPI.Types.Cat
import {-# SOURCE #-} OpenAPI.Types.PetByType

-- | Defines the object schema located at @components.schemas.PetByAge@ in the specification.
-- 
-- 
data PetByAge = PetByAge {
  -- | age
  petByAgeAge :: GHC.Types.Int
  -- | another_relative
  , petByAgeAnother_relative :: (GHC.Maybe.Maybe PetByAgeAnother_relativeVariants)
  -- | first_relative
  , petByAgeFirst_relative :: (GHC.Maybe.Maybe PetByAgeFirst_relative)
  -- | nickname
  , petByAgeNickname :: (GHC.Maybe.Maybe Data.Text.Internal.Text)
  -- | relative
  , petByAgeRelative :: (GHC.Maybe.Maybe PetByAgeRelativeVariants)
  } deriving (GHC.Show.Show
  , GHC.Classes.Eq)
instance Data.Aeson.Types.ToJSON.ToJSON PetByAge
    where {toJSON obj = Data.Aeson.Types.Internal.object (Data.Foldable.concat (["age" Data.Aeson.Types.ToJSON..= petByAgeAge obj] : Data.Maybe.maybe GHC.Base.mempty (GHC.Base.pure GHC.Base.. ("another_relative" Data.Aeson.Types.ToJSON..=)) (petByAgeAnother_relative obj) : Data.Maybe.maybe GHC.Base.mempty (GHC.Base.pure GHC.Base.. ("first_relative" Data.Aeson.Types.ToJSON..=)) (petByAgeFirst_relative obj) : Data.Maybe.maybe GHC.Base.mempty (GHC.Base.pure GHC.Base.. ("nickname" Data.Aeson.Types.ToJSON..=)) (petByAgeNickname obj) : Data.Maybe.maybe GHC.Base.mempty (GHC.Base.pure GHC.Base.. ("relative" Data.Aeson.Types.ToJSON..=)) (petByAgeRelative obj) : GHC.Base.mempty));
           toEncoding obj = Data.Aeson.Encoding.Internal.pairs (GHC.Base.mconcat (Data.Foldable.concat (["age" Data.Aeson.Types.ToJSON..= petByAgeAge obj] : Data.Maybe.maybe GHC.Base.mempty (GHC.Base.pure GHC.Base.. ("another_relative" Data.Aeson.Types.ToJSON..=)) (petByAgeAnother_relative obj) : Data.Maybe.maybe GHC.Base.mempty (GHC.Base.pure GHC.Base.. ("first_relative" Data.Aeson.Types.ToJSON..=)) (petByAgeFirst_relative obj) : Data.Maybe.maybe GHC.Base.mempty (GHC.Base.pure GHC.Base.. ("nickname" Data.Aeson.Types.ToJSON..=)) (petByAgeNickname obj) : Data.Maybe.maybe GHC.Base.mempty (GHC.Base.pure GHC.Base.. ("relative" Data.Aeson.Types.ToJSON..=)) (petByAgeRelative obj) : GHC.Base.mempty)))}
instance Data.Aeson.Types.FromJSON.FromJSON PetByAge
    where {parseJSON = Data.Aeson.Types.FromJSON.withObject "PetByAge" (\obj -> ((((GHC.Base.pure PetByAge GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..: "age")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:! "another_relative")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:! "first_relative")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:! "nickname")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:! "relative"))}
-- | Create a new 'PetByAge' with all required fields.
mkPetByAge :: GHC.Types.Int -- ^ 'petByAgeAge'
  -> PetByAge
mkPetByAge petByAgeAge = PetByAge{petByAgeAge = petByAgeAge,
                                  petByAgeAnother_relative = GHC.Maybe.Nothing,
                                  petByAgeFirst_relative = GHC.Maybe.Nothing,
                                  petByAgeNickname = GHC.Maybe.Nothing,
                                  petByAgeRelative = GHC.Maybe.Nothing}
-- | Defines the object schema located at @components.schemas.PetByAge.properties.another_relative.oneOf@ in the specification.
-- 
-- 
data PetByAgeAnother_relativeOneOf5 = PetByAgeAnother_relativeOneOf5 {
  -- | hunts
  petByAgeAnother_relativeOneOf5Hunts :: (GHC.Maybe.Maybe GHC.Types.Bool)
  -- | pet_type
  , petByAgeAnother_relativeOneOf5Pet_type :: (GHC.Maybe.Maybe Data.Text.Internal.Text)
  } deriving (GHC.Show.Show
  , GHC.Classes.Eq)
instance Data.Aeson.Types.ToJSON.ToJSON PetByAgeAnother_relativeOneOf5
    where {toJSON obj = Data.Aeson.Types.Internal.object (Data.Foldable.concat (Data.Maybe.maybe GHC.Base.mempty (GHC.Base.pure GHC.Base.. ("hunts" Data.Aeson.Types.ToJSON..=)) (petByAgeAnother_relativeOneOf5Hunts obj) : Data.Maybe.maybe GHC.Base.mempty (GHC.Base.pure GHC.Base.. ("pet_type" Data.Aeson.Types.ToJSON..=)) (petByAgeAnother_relativeOneOf5Pet_type obj) : GHC.Base.mempty));
           toEncoding obj = Data.Aeson.Encoding.Internal.pairs (GHC.Base.mconcat (Data.Foldable.concat (Data.Maybe.maybe GHC.Base.mempty (GHC.Base.pure GHC.Base.. ("hunts" Data.Aeson.Types.ToJSON..=)) (petByAgeAnother_relativeOneOf5Hunts obj) : Data.Maybe.maybe GHC.Base.mempty (GHC.Base.pure GHC.Base.. ("pet_type" Data.Aeson.Types.ToJSON..=)) (petByAgeAnother_relativeOneOf5Pet_type obj) : GHC.Base.mempty)))}
instance Data.Aeson.Types.FromJSON.FromJSON PetByAgeAnother_relativeOneOf5
    where {parseJSON = Data.Aeson.Types.FromJSON.withObject "PetByAgeAnother_relativeOneOf5" (\obj -> (GHC.Base.pure PetByAgeAnother_relativeOneOf5 GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:! "hunts")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:! "pet_type"))}
-- | Create a new 'PetByAgeAnother_relativeOneOf5' with all required fields.
mkPetByAgeAnother_relativeOneOf5 :: PetByAgeAnother_relativeOneOf5
mkPetByAgeAnother_relativeOneOf5 = PetByAgeAnother_relativeOneOf5{petByAgeAnother_relativeOneOf5Hunts = GHC.Maybe.Nothing,
                                                                  petByAgeAnother_relativeOneOf5Pet_type = GHC.Maybe.Nothing}
-- | Defines the oneOf schema located at @components.schemas.PetByAge.properties.another_relative.oneOf@ in the specification.
-- 
-- 
data PetByAgeAnother_relativeVariants =
   PetByAgeAnother_relativeEmptyString -- ^ Represents the JSON value @""@
  | PetByAgeAnother_relativeTest -- ^ Represents the JSON value @"test"@
  | PetByAgeAnother_relativeCat Cat
  | PetByAgeAnother_relativePetByType PetByType
  | PetByAgeAnother_relativeText Data.Text.Internal.Text
  | PetByAgeAnother_relativeListTText ([Data.Text.Internal.Text])
  | PetByAgeAnother_relativePetByAgeAnother_relativeOneOf5 PetByAgeAnother_relativeOneOf5
  deriving (GHC.Show.Show, GHC.Classes.Eq)
instance Data.Aeson.Types.ToJSON.ToJSON PetByAgeAnother_relativeVariants
    where {toJSON (PetByAgeAnother_relativeCat a) = Data.Aeson.Types.ToJSON.toJSON a;
           toJSON (PetByAgeAnother_relativePetByType a) = Data.Aeson.Types.ToJSON.toJSON a;
           toJSON (PetByAgeAnother_relativeText a) = Data.Aeson.Types.ToJSON.toJSON a;
           toJSON (PetByAgeAnother_relativeListTText a) = Data.Aeson.Types.ToJSON.toJSON a;
           toJSON (PetByAgeAnother_relativePetByAgeAnother_relativeOneOf5 a) = Data.Aeson.Types.ToJSON.toJSON a;
           toJSON (PetByAgeAnother_relativeEmptyString) = "";
           toJSON (PetByAgeAnother_relativeTest) = "test"}
instance Data.Aeson.Types.FromJSON.FromJSON PetByAgeAnother_relativeVariants
    where {parseJSON val = if | val GHC.Classes.== "" -> GHC.Base.pure PetByAgeAnother_relativeEmptyString
                              | val GHC.Classes.== "test" -> GHC.Base.pure PetByAgeAnother_relativeTest
                              | GHC.Base.otherwise -> case (PetByAgeAnother_relativeCat Data.Functor.<$> Data.Aeson.Types.FromJSON.fromJSON val) GHC.Base.<|> ((PetByAgeAnother_relativePetByType Data.Functor.<$> Data.Aeson.Types.FromJSON.fromJSON val) GHC.Base.<|> ((PetByAgeAnother_relativeText Data.Functor.<$> Data.Aeson.Types.FromJSON.fromJSON val) GHC.Base.<|> ((PetByAgeAnother_relativeListTText Data.Functor.<$> Data.Aeson.Types.FromJSON.fromJSON val) GHC.Base.<|> ((PetByAgeAnother_relativePetByAgeAnother_relativeOneOf5 Data.Functor.<$> Data.Aeson.Types.FromJSON.fromJSON val) GHC.Base.<|> Data.Aeson.Types.Internal.Error "No variant matched")))) of
                                                      {Data.Aeson.Types.Internal.Success a -> GHC.Base.pure a;
                                                       Data.Aeson.Types.Internal.Error a -> Control.Monad.Fail.fail a}}
-- | Defines the object schema located at @components.schemas.PetByAge.properties.first_relative.allOf@ in the specification.
-- 
-- 
data PetByAgeFirst_relative = PetByAgeFirst_relative {
  -- | age
  petByAgeFirst_relativeAge :: (GHC.Maybe.Maybe GHC.Types.Int)
  -- | another_relative
  , petByAgeFirst_relativeAnother_relative :: (GHC.Maybe.Maybe PetByAgeFirst_relativeAnother_relativeVariants)
  -- | hunts
  , petByAgeFirst_relativeHunts :: (GHC.Maybe.Maybe GHC.Types.Bool)
  -- | pet_type
  , petByAgeFirst_relativePet_type :: PetByAgeFirst_relativePet_type
  -- | relative
  , petByAgeFirst_relativeRelative :: (GHC.Maybe.Maybe PetByAgeFirst_relativeRelativeVariants)
  } deriving (GHC.Show.Show
  , GHC.Classes.Eq)
instance Data.Aeson.Types.ToJSON.ToJSON PetByAgeFirst_relative
    where {toJSON obj = Data.Aeson.Types.Internal.object (Data.Foldable.concat (Data.Maybe.maybe GHC.Base.mempty (GHC.Base.pure GHC.Base.. ("age" Data.Aeson.Types.ToJSON..=)) (petByAgeFirst_relativeAge obj) : Data.Maybe.maybe GHC.Base.mempty (GHC.Base.pure GHC.Base.. ("another_relative" Data.Aeson.Types.ToJSON..=)) (petByAgeFirst_relativeAnother_relative obj) : Data.Maybe.maybe GHC.Base.mempty (GHC.Base.pure GHC.Base.. ("hunts" Data.Aeson.Types.ToJSON..=)) (petByAgeFirst_relativeHunts obj) : ["pet_type" Data.Aeson.Types.ToJSON..= petByAgeFirst_relativePet_type obj] : Data.Maybe.maybe GHC.Base.mempty (GHC.Base.pure GHC.Base.. ("relative" Data.Aeson.Types.ToJSON..=)) (petByAgeFirst_relativeRelative obj) : GHC.Base.mempty));
           toEncoding obj = Data.Aeson.Encoding.Internal.pairs (GHC.Base.mconcat (Data.Foldable.concat (Data.Maybe.maybe GHC.Base.mempty (GHC.Base.pure GHC.Base.. ("age" Data.Aeson.Types.ToJSON..=)) (petByAgeFirst_relativeAge obj) : Data.Maybe.maybe GHC.Base.mempty (GHC.Base.pure GHC.Base.. ("another_relative" Data.Aeson.Types.ToJSON..=)) (petByAgeFirst_relativeAnother_relative obj) : Data.Maybe.maybe GHC.Base.mempty (GHC.Base.pure GHC.Base.. ("hunts" Data.Aeson.Types.ToJSON..=)) (petByAgeFirst_relativeHunts obj) : ["pet_type" Data.Aeson.Types.ToJSON..= petByAgeFirst_relativePet_type obj] : Data.Maybe.maybe GHC.Base.mempty (GHC.Base.pure GHC.Base.. ("relative" Data.Aeson.Types.ToJSON..=)) (petByAgeFirst_relativeRelative obj) : GHC.Base.mempty)))}
instance Data.Aeson.Types.FromJSON.FromJSON PetByAgeFirst_relative
    where {parseJSON = Data.Aeson.Types.FromJSON.withObject "PetByAgeFirst_relative" (\obj -> ((((GHC.Base.pure PetByAgeFirst_relative GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:! "age")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:! "another_relative")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:! "hunts")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..: "pet_type")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:! "relative"))}
-- | Create a new 'PetByAgeFirst_relative' with all required fields.
mkPetByAgeFirst_relative :: PetByAgeFirst_relativePet_type -- ^ 'petByAgeFirst_relativePet_type'
  -> PetByAgeFirst_relative
mkPetByAgeFirst_relative petByAgeFirst_relativePet_type = PetByAgeFirst_relative{petByAgeFirst_relativeAge = GHC.Maybe.Nothing,
                                                                                 petByAgeFirst_relativeAnother_relative = GHC.Maybe.Nothing,
                                                                                 petByAgeFirst_relativeHunts = GHC.Maybe.Nothing,
                                                                                 petByAgeFirst_relativePet_type = petByAgeFirst_relativePet_type,
                                                                                 petByAgeFirst_relativeRelative = GHC.Maybe.Nothing}
-- | Defines the oneOf schema located at @components.schemas.PetByAge.properties.first_relative.allOf.properties.another_relative.oneOf@ in the specification.
-- 
-- 
data PetByAgeFirst_relativeAnother_relativeVariants =
   PetByAgeFirst_relativeAnother_relativeCat Cat
  | PetByAgeFirst_relativeAnother_relativePetByType PetByType
  | PetByAgeFirst_relativeAnother_relativeText Data.Text.Internal.Text
  deriving (GHC.Show.Show, GHC.Classes.Eq)
instance Data.Aeson.Types.ToJSON.ToJSON PetByAgeFirst_relativeAnother_relativeVariants
    where {toJSON (PetByAgeFirst_relativeAnother_relativeCat a) = Data.Aeson.Types.ToJSON.toJSON a;
           toJSON (PetByAgeFirst_relativeAnother_relativePetByType a) = Data.Aeson.Types.ToJSON.toJSON a;
           toJSON (PetByAgeFirst_relativeAnother_relativeText a) = Data.Aeson.Types.ToJSON.toJSON a}
instance Data.Aeson.Types.FromJSON.FromJSON PetByAgeFirst_relativeAnother_relativeVariants
    where {parseJSON val = case (PetByAgeFirst_relativeAnother_relativeCat Data.Functor.<$> Data.Aeson.Types.FromJSON.fromJSON val) GHC.Base.<|> ((PetByAgeFirst_relativeAnother_relativePetByType Data.Functor.<$> Data.Aeson.Types.FromJSON.fromJSON val) GHC.Base.<|> ((PetByAgeFirst_relativeAnother_relativeText Data.Functor.<$> Data.Aeson.Types.FromJSON.fromJSON val) GHC.Base.<|> Data.Aeson.Types.Internal.Error "No variant matched")) of
                           {Data.Aeson.Types.Internal.Success a -> GHC.Base.pure a;
                            Data.Aeson.Types.Internal.Error a -> Control.Monad.Fail.fail a}}
-- | Defines the enum schema located at @components.schemas.PetByAge.properties.first_relative.allOf.properties.pet_type@ in the specification.
-- 
-- 
data PetByAgeFirst_relativePet_type =
   PetByAgeFirst_relativePet_typeOther Data.Aeson.Types.Internal.Value -- ^ This case is used if the value encountered during decoding does not match any of the provided cases in the specification.
  | PetByAgeFirst_relativePet_typeTyped Data.Text.Internal.Text -- ^ This constructor can be used to send values to the server which are not present in the specification yet.
  | PetByAgeFirst_relativePet_typeEnumCat -- ^ Represents the JSON value @"Cat"@
  | PetByAgeFirst_relativePet_typeEnumDog -- ^ Represents the JSON value @"Dog"@
  deriving (GHC.Show.Show, GHC.Classes.Eq)
instance Data.Aeson.Types.ToJSON.ToJSON PetByAgeFirst_relativePet_type
    where {toJSON (PetByAgeFirst_relativePet_typeOther val) = val;
           toJSON (PetByAgeFirst_relativePet_typeTyped val) = Data.Aeson.Types.ToJSON.toJSON val;
           toJSON (PetByAgeFirst_relativePet_typeEnumCat) = "Cat";
           toJSON (PetByAgeFirst_relativePet_typeEnumDog) = "Dog"}
instance Data.Aeson.Types.FromJSON.FromJSON PetByAgeFirst_relativePet_type
    where {parseJSON val = GHC.Base.pure (if | val GHC.Classes.== "Cat" -> PetByAgeFirst_relativePet_typeEnumCat
                                             | val GHC.Classes.== "Dog" -> PetByAgeFirst_relativePet_typeEnumDog
                                             | GHC.Base.otherwise -> PetByAgeFirst_relativePet_typeOther val)}
-- | Defines the oneOf schema located at @components.schemas.PetByAge.properties.first_relative.allOf.properties.relative.anyOf@ in the specification.
-- 
-- 
data PetByAgeFirst_relativeRelativeVariants =
   PetByAgeFirst_relativeRelativeCat Cat
  | PetByAgeFirst_relativeRelativePetByType PetByType
  | PetByAgeFirst_relativeRelativeText Data.Text.Internal.Text
  deriving (GHC.Show.Show, GHC.Classes.Eq)
instance Data.Aeson.Types.ToJSON.ToJSON PetByAgeFirst_relativeRelativeVariants
    where {toJSON (PetByAgeFirst_relativeRelativeCat a) = Data.Aeson.Types.ToJSON.toJSON a;
           toJSON (PetByAgeFirst_relativeRelativePetByType a) = Data.Aeson.Types.ToJSON.toJSON a;
           toJSON (PetByAgeFirst_relativeRelativeText a) = Data.Aeson.Types.ToJSON.toJSON a}
instance Data.Aeson.Types.FromJSON.FromJSON PetByAgeFirst_relativeRelativeVariants
    where {parseJSON val = case (PetByAgeFirst_relativeRelativeCat Data.Functor.<$> Data.Aeson.Types.FromJSON.fromJSON val) GHC.Base.<|> ((PetByAgeFirst_relativeRelativePetByType Data.Functor.<$> Data.Aeson.Types.FromJSON.fromJSON val) GHC.Base.<|> ((PetByAgeFirst_relativeRelativeText Data.Functor.<$> Data.Aeson.Types.FromJSON.fromJSON val) GHC.Base.<|> Data.Aeson.Types.Internal.Error "No variant matched")) of
                           {Data.Aeson.Types.Internal.Success a -> GHC.Base.pure a;
                            Data.Aeson.Types.Internal.Error a -> Control.Monad.Fail.fail a}}
-- | Defines the oneOf schema located at @components.schemas.PetByAge.properties.relative.anyOf@ in the specification.
-- 
-- 
data PetByAgeRelativeVariants =
   PetByAgeRelativeCat Cat
  | PetByAgeRelativePetByType PetByType
  | PetByAgeRelativeText Data.Text.Internal.Text
  deriving (GHC.Show.Show, GHC.Classes.Eq)
instance Data.Aeson.Types.ToJSON.ToJSON PetByAgeRelativeVariants
    where {toJSON (PetByAgeRelativeCat a) = Data.Aeson.Types.ToJSON.toJSON a;
           toJSON (PetByAgeRelativePetByType a) = Data.Aeson.Types.ToJSON.toJSON a;
           toJSON (PetByAgeRelativeText a) = Data.Aeson.Types.ToJSON.toJSON a}
instance Data.Aeson.Types.FromJSON.FromJSON PetByAgeRelativeVariants
    where {parseJSON val = case (PetByAgeRelativeCat Data.Functor.<$> Data.Aeson.Types.FromJSON.fromJSON val) GHC.Base.<|> ((PetByAgeRelativePetByType Data.Functor.<$> Data.Aeson.Types.FromJSON.fromJSON val) GHC.Base.<|> ((PetByAgeRelativeText Data.Functor.<$> Data.Aeson.Types.FromJSON.fromJSON val) GHC.Base.<|> Data.Aeson.Types.Internal.Error "No variant matched")) of
                           {Data.Aeson.Types.Internal.Success a -> GHC.Base.pure a;
                            Data.Aeson.Types.Internal.Error a -> Control.Monad.Fail.fail a}}
