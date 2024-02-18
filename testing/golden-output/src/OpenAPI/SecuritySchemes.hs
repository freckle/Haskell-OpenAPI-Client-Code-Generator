-- CHANGE WITH CAUTION: This is a generated code file generated by https://github.com/Haskell-OpenAPI-Code-Generator/Haskell-OpenAPI-Client-Code-Generator.

{-# LANGUAGE OverloadedStrings #-}

-- | Contains all supported security schemes defined in the specification
module OpenAPI.SecuritySchemes where

import qualified Data.Text as Data.Text.Internal
import qualified GHC.Base
import qualified GHC.Classes
import qualified GHC.Show
import qualified Network.HTTP.Client as Network.HTTP.Client.Request
import qualified Network.HTTP.Simple
import qualified OpenAPI.Common

-- | Used to pass the authentication information for BasicAuthentication to 'basicAuthenticationSecurityScheme'.
data BasicAuthenticationData
    = BasicAuthenticationData {basicAuthenticationDataUsername :: Data.Text.Internal.Text,
                               basicAuthenticationDataPassword :: Data.Text.Internal.Text}
    deriving (GHC.Show.Show, GHC.Classes.Ord, GHC.Classes.Eq)

-- | Use this security scheme to use basic authentication for a request. Should be used in a 'OpenAPI.Common.Configuration'.
-- 
-- Basic HTTP authentication. Allowed headers-- Authorization: Basic \<api_key> | Authorization: Basic \<base64 hash of \`api_key:\`>
-- 
-- @
-- 'OpenAPI.Configuration.defaultConfiguration'
--   { configSecurityScheme =
--       'basicAuthenticationSecurityScheme' 'BasicAuthenticationData'
--         { 'basicAuthenticationDataUsername' = "user",
--           'basicAuthenticationDataPassword' = "pw"
--         }
--   }
-- @
basicAuthenticationSecurityScheme :: BasicAuthenticationData ->
                                     OpenAPI.Common.SecurityScheme
basicAuthenticationSecurityScheme = \basicAuth_0 -> Network.HTTP.Client.Request.applyBasicAuth (OpenAPI.Common.textToByte GHC.Base.$ basicAuthenticationDataUsername basicAuth_0) (OpenAPI.Common.textToByte GHC.Base.$ basicAuthenticationDataPassword basicAuth_0)

-- | Use this security scheme to use bearer authentication for a request. Should be used in a 'OpenAPI.Common.Configuration'.
-- 
-- Bearer HTTP authentication. Allowed headers-- Authorization: Bearer \<api_key>
-- 
-- @
-- 'OpenAPI.Configuration.defaultConfiguration'
--   { configSecurityScheme = 'bearerAuthenticationSecurityScheme' "token"
--   }
-- @
bearerAuthenticationSecurityScheme :: Data.Text.Internal.Text ->
                                      OpenAPI.Common.SecurityScheme
bearerAuthenticationSecurityScheme = \token_1 -> Network.HTTP.Simple.addRequestHeader "Authorization" GHC.Base.$ (OpenAPI.Common.textToByte GHC.Base.$ ("Bearer " GHC.Base.<> token_1))
