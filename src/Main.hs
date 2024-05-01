{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}

import Yesod
import Prelude
import Data.Aeson()
import GHC.Generics
-- import Data.Text ( Text )
import Data.Text.Lazy
-- import System.Environment ( getArgs )
-- import Data.Text.Lazy.IO ( writeFile )
import Data.Aeson.Text (encodeToLazyText)

-- project imports
import qualified PhpParser

data SourceFile
   = SourceFile
     {
         filename :: String,
         content :: String
     }
     deriving ( Generic, ToJSON, FromJSON )

data Healthy = Healthy Bool deriving ( Generic )

data Error = Error String String deriving ( Generic )

-- | indicate a parse error 
instance ToJSON Error where toJSON (Error status message) = object [ "status" .= status, "message" .= message ]

-- | This is just for the health check ...
instance ToJSON Healthy where toJSON (Healthy status) = object [ "healthy" .= status ]

data App = App

mkYesod "App" [parseRoutes|
/from/php/to/dhscanner/ast FromPhpR POST
/from/js/to/dhscanner/ast FromJavascriptR POST
/healthcheck HealthcheckR GET
|]

instance Yesod App

getHealthcheckR :: Handler Value
getHealthcheckR = returnJson $ Healthy True

postFromPhpR :: Handler Value
postFromPhpR = do
    src <- requireCheckJsonBody :: Handler SourceFile
    case PhpParser.parseProgram (filename src) (content src) of
        Left errorMsg -> returnJson (Error "FAILED" errorMsg)
        Right ast -> returnJson ast

postFromJavascriptR :: Handler Value
postFromJavascriptR = do
    src <- requireCheckJsonBody :: Handler SourceFile
    case JsParser.parseProgram (filename src) (content src) of
        Left errorMsg -> returnJson (Error "FAILED" errorMsg)
        Right ast -> returnJson ast

main :: IO ()
main = warp 3000 App
