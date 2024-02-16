{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Functionality to Generate Haskell Code out of an OpenAPI definition File
module OpenAPI.Generate where

import Control.Applicative ((<|>))
import Data.Bifunctor (first)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Aeson as Aeson
import qualified Data.Yaml as Yaml
import qualified OpenAPI.Generate.IO as OAI
import qualified OpenAPI.Generate.OptParse as OAO
import qualified OpenAPI.Generate.OptParse.Types as OAO
import qualified OpenAPI.Generate.Types as OAT
import System.Exit
import System.FilePath (takeExtension)

-- | Decodes an OpenAPI File
decodeOpenApi :: Text -> Maybe OAO.SpecificationFormat -> IO OAT.OpenApiSpecification
decodeOpenApi fileNameText mFormat = do
  res <- case mFormat <|> OAO.specificationFormatByExtension (takeExtension fileName) of
    Just OAO.SpecificationFormatYaml -> first Yaml.prettyPrintParseException <$> Yaml.decodeFileEither fileName
    Just OAO.SpecificationFormatJSON -> Aeson.eitherDecodeFileStrict fileName
    Nothing -> undefined
  case res of
    Left exc -> die $ "Could not parse OpenAPI specification '" <> fileName <> "': " <> exc
    Right o -> pure o
 where
  fileName = T.unpack fileNameText

-- | Run the generator as CLI
runGenerator :: IO ()
runGenerator = do
  settings <- OAO.getSettings
  spec <- decodeOpenApi (OAO.settingOpenApiSpecification settings) $ OAO.settingSpecificationFormat settings
  outFiles@OAI.OutputFiles {..} <- OAI.generateFilesToCreate spec settings
  if OAO.settingDryRun settings
    then
      mapM_
        ( \(file, content) -> do
            putStrLn $ "File: " <> file
            putStrLn "---"
            putStrLn content
            putStrLn "---\n\n"
        )
        outputFilesModuleFiles
    else do
      proceed <- OAI.permitProceed settings
      if proceed
        then do
          OAI.writeFiles settings outFiles
          putStrLn "finished"
        else putStrLn "aborted"
