{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Contains the types generated from the schema Dog
module OpenAPI.Types.Dog where

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

-- | Defines the data type for the schema Dog
-- 
-- 
data Dog = Dog {
  -- | bark
  dogBark :: (GHC.Maybe.Maybe GHC.Types.Bool)
  -- | binary
  , dogBinary :: (GHC.Maybe.Maybe OpenAPI.Common.JsonByteString)
  -- | breed
  , dogBreed :: (GHC.Maybe.Maybe DogBreed)
  -- | byte
  , dogByte :: (GHC.Maybe.Maybe OpenAPI.Common.JsonByteString)
  -- | double
  , dogDouble :: (GHC.Maybe.Maybe GHC.Types.Double)
  -- | father
  , dogFather :: (GHC.Maybe.Maybe DogFather)
  -- | float
  , dogFloat :: (GHC.Maybe.Maybe GHC.Types.Float)
  -- | int32
  , dogInt32 :: (GHC.Maybe.Maybe GHC.Int.Int32)
  -- | int64
  , dogInt64 :: (GHC.Maybe.Maybe GHC.Int.Int64)
  -- | integer
  , dogInteger :: (GHC.Maybe.Maybe GHC.Integer.Type.Integer)
  -- | number
  , dogNumber :: (GHC.Maybe.Maybe GHC.Types.Double)
  -- | pet_type
  , dogPet_type :: (GHC.Maybe.Maybe DogPet_type)
  -- | secondFather
  , dogSecondFather :: (GHC.Maybe.Maybe DogSecondFather)
  -- | str
  -- 
  -- Constraints:
  -- 
  -- * Maximum length of 244
  -- * Minimum length of 100
  , dogStr :: (GHC.Maybe.Maybe GHC.Base.String)
  -- | strDate
  , dogStrDate :: (GHC.Maybe.Maybe GHC.Base.String)
  -- | strDateTime
  , dogStrDateTime :: (GHC.Maybe.Maybe GHC.Base.String)
  -- | tags
  , dogTags :: (GHC.Maybe.Maybe ([] GHC.Base.String))
  } deriving (GHC.Show.Show
  , GHC.Classes.Eq)
instance Data.Aeson.ToJSON Dog
    where toJSON obj = Data.Aeson.object ((Data.Aeson..=) "bark" (dogBark obj) : (Data.Aeson..=) "binary" (dogBinary obj) : (Data.Aeson..=) "breed" (dogBreed obj) : (Data.Aeson..=) "byte" (dogByte obj) : (Data.Aeson..=) "double" (dogDouble obj) : (Data.Aeson..=) "father" (dogFather obj) : (Data.Aeson..=) "float" (dogFloat obj) : (Data.Aeson..=) "int32" (dogInt32 obj) : (Data.Aeson..=) "int64" (dogInt64 obj) : (Data.Aeson..=) "integer" (dogInteger obj) : (Data.Aeson..=) "number" (dogNumber obj) : (Data.Aeson..=) "pet_type" (dogPet_type obj) : (Data.Aeson..=) "secondFather" (dogSecondFather obj) : (Data.Aeson..=) "str" (dogStr obj) : (Data.Aeson..=) "strDate" (dogStrDate obj) : (Data.Aeson..=) "strDateTime" (dogStrDateTime obj) : (Data.Aeson..=) "tags" (dogTags obj) : [])
          toEncoding obj = Data.Aeson.pairs ((Data.Aeson..=) "bark" (dogBark obj) GHC.Base.<> ((Data.Aeson..=) "binary" (dogBinary obj) GHC.Base.<> ((Data.Aeson..=) "breed" (dogBreed obj) GHC.Base.<> ((Data.Aeson..=) "byte" (dogByte obj) GHC.Base.<> ((Data.Aeson..=) "double" (dogDouble obj) GHC.Base.<> ((Data.Aeson..=) "father" (dogFather obj) GHC.Base.<> ((Data.Aeson..=) "float" (dogFloat obj) GHC.Base.<> ((Data.Aeson..=) "int32" (dogInt32 obj) GHC.Base.<> ((Data.Aeson..=) "int64" (dogInt64 obj) GHC.Base.<> ((Data.Aeson..=) "integer" (dogInteger obj) GHC.Base.<> ((Data.Aeson..=) "number" (dogNumber obj) GHC.Base.<> ((Data.Aeson..=) "pet_type" (dogPet_type obj) GHC.Base.<> ((Data.Aeson..=) "secondFather" (dogSecondFather obj) GHC.Base.<> ((Data.Aeson..=) "str" (dogStr obj) GHC.Base.<> ((Data.Aeson..=) "strDate" (dogStrDate obj) GHC.Base.<> ((Data.Aeson..=) "strDateTime" (dogStrDateTime obj) GHC.Base.<> (Data.Aeson..=) "tags" (dogTags obj)))))))))))))))))
instance Data.Aeson.Types.FromJSON.FromJSON Dog
    where parseJSON = Data.Aeson.Types.FromJSON.withObject "Dog" (\obj -> ((((((((((((((((GHC.Base.pure Dog GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:? "bark")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:? "binary")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:? "breed")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:? "byte")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:? "double")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:? "father")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:? "float")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:? "int32")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:? "int64")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:? "integer")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:? "number")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:? "pet_type")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:? "secondFather")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:? "str")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:? "strDate")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:? "strDateTime")) GHC.Base.<*> (obj Data.Aeson.Types.FromJSON..:? "tags"))
-- | Defines the enum schema DogBreed
-- 
-- 
data DogBreed
    = DogBreedEnumOther Data.Aeson.Types.Internal.Value
    | DogBreedEnumTyped GHC.Base.String
    | DogBreedEnumString_Dingo
    | DogBreedEnumString_Husky
    | DogBreedEnumString_Retriever
    | DogBreedEnumString_Shepherd
    deriving (GHC.Show.Show, GHC.Classes.Eq)
instance Data.Aeson.ToJSON DogBreed
    where toJSON (DogBreedEnumOther patternName) = Data.Aeson.Types.ToJSON.toJSON patternName
          toJSON (DogBreedEnumTyped patternName) = Data.Aeson.Types.ToJSON.toJSON patternName
          toJSON (DogBreedEnumString_Dingo) = Data.Aeson.Types.Internal.String GHC.Base.$ Data.Text.pack "Dingo"
          toJSON (DogBreedEnumString_Husky) = Data.Aeson.Types.Internal.String GHC.Base.$ Data.Text.pack "Husky"
          toJSON (DogBreedEnumString_Retriever) = Data.Aeson.Types.Internal.String GHC.Base.$ Data.Text.pack "Retriever"
          toJSON (DogBreedEnumString_Shepherd) = Data.Aeson.Types.Internal.String GHC.Base.$ Data.Text.pack "Shepherd"
instance Data.Aeson.FromJSON DogBreed
    where parseJSON val = GHC.Base.pure (if val GHC.Classes.== (Data.Aeson.Types.Internal.String GHC.Base.$ Data.Text.pack "Dingo")
                                          then DogBreedEnumString_Dingo
                                          else if val GHC.Classes.== (Data.Aeson.Types.Internal.String GHC.Base.$ Data.Text.pack "Husky")
                                                then DogBreedEnumString_Husky
                                                else if val GHC.Classes.== (Data.Aeson.Types.Internal.String GHC.Base.$ Data.Text.pack "Retriever")
                                                      then DogBreedEnumString_Retriever
                                                      else if val GHC.Classes.== (Data.Aeson.Types.Internal.String GHC.Base.$ Data.Text.pack "Shepherd")
                                                            then DogBreedEnumString_Shepherd
                                                            else DogBreedEnumOther val)
-- | Defines the data type for the schema DogFather
-- 
-- 
data DogFather = DogFather {
  
  } deriving (GHC.Show.Show
  , GHC.Classes.Eq)
instance Data.Aeson.ToJSON DogFather
    where toJSON obj = Data.Aeson.object []
          toEncoding obj = Data.Aeson.pairs ((Data.Aeson..=) "string" ("string" :: GHC.Base.String))
instance Data.Aeson.Types.FromJSON.FromJSON DogFather
    where parseJSON = Data.Aeson.Types.FromJSON.withObject "DogFather" (\obj -> GHC.Base.pure DogFather)
-- | Defines the data type for the schema DogPet_type
-- 
-- 
data DogPet_type = DogPet_type {
  
  } deriving (GHC.Show.Show
  , GHC.Classes.Eq)
instance Data.Aeson.ToJSON DogPet_type
    where toJSON obj = Data.Aeson.object []
          toEncoding obj = Data.Aeson.pairs ((Data.Aeson..=) "string" ("string" :: GHC.Base.String))
instance Data.Aeson.Types.FromJSON.FromJSON DogPet_type
    where parseJSON = Data.Aeson.Types.FromJSON.withObject "DogPet_type" (\obj -> GHC.Base.pure DogPet_type)
-- | Defines the data type for the schema DogSecondFather
-- 
-- 
data DogSecondFather = DogSecondFather {
  
  } deriving (GHC.Show.Show
  , GHC.Classes.Eq)
instance Data.Aeson.ToJSON DogSecondFather
    where toJSON obj = Data.Aeson.object []
          toEncoding obj = Data.Aeson.pairs ((Data.Aeson..=) "string" ("string" :: GHC.Base.String))
instance Data.Aeson.Types.FromJSON.FromJSON DogSecondFather
    where parseJSON = Data.Aeson.Types.FromJSON.withObject "DogSecondFather" (\obj -> GHC.Base.pure DogSecondFather)