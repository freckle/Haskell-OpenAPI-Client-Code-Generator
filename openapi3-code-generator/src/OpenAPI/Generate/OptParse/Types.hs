module OpenAPI.Generate.OptParse.Types
  ( FixedValueStrategy (..),
    SpecificationFormat(..),
    specificationFormatByExtension
  )
where

import Autodocodec

data FixedValueStrategy = FixedValueStrategyExclude | FixedValueStrategyInclude
  deriving (Eq, Bounded, Enum)

instance Show FixedValueStrategy where
  show FixedValueStrategyExclude = "exclude"
  show FixedValueStrategyInclude = "include"

instance Read FixedValueStrategy where
  readsPrec _ ('e' : 'x' : 'c' : 'l' : 'u' : 'd' : 'e' : rest) = [(FixedValueStrategyExclude, rest)]
  readsPrec _ ('i' : 'n' : 'c' : 'l' : 'u' : 'd' : 'e' : rest) = [(FixedValueStrategyInclude, rest)]
  readsPrec _ _ = []

instance HasCodec FixedValueStrategy where
  codec = shownBoundedEnumCodec

data SpecificationFormat = SpecificationFormatYaml | SpecificationFormatJSON
  deriving (Eq, Bounded, Enum)

instance Show SpecificationFormat where
  show SpecificationFormatYaml = "yaml"
  show SpecificationFormatJSON = "json"

instance Read SpecificationFormat where
  readsPrec _ ('y' : 'a' : 'm' : 'l': rest) = [(SpecificationFormatYaml, rest)]
  readsPrec _ ('j' : 's' : 'o' : 'n': rest) = [(SpecificationFormatJSON, rest)]
  readsPrec _ _ = []

instance HasCodec SpecificationFormat where
  codec = shownBoundedEnumCodec

specificationFormatByExtension :: String -> Maybe SpecificationFormat
specificationFormatByExtension ".yaml" = Just SpecificationFormatYaml
specificationFormatByExtension ".yml" = Just SpecificationFormatYaml
specificationFormatByExtension ".json" = Just SpecificationFormatJSON
specificationFormatByExtension _ = Nothing
