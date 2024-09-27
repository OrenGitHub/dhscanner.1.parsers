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

-- project imports
import qualified JsParser
import qualified TsParser
import qualified PyParser
import qualified RbParser
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
/from/py/to/dhscanner/ast FromPyR POST
/from/rb/to/dhscanner/ast FromRbR POST
/from/js/to/dhscanner/ast FromJsR POST
/from/ts/to/dhscanner/ast FromTsR POST
/healthcheck HealthcheckR GET
|]

instance Yesod App where maximumContentLength = \_app -> (\_anyRouteReally -> Just 80000000)

getHealthcheckR :: Handler Value
getHealthcheckR = returnJson $ Healthy True

postFromPhpR :: Handler Value
postFromPhpR = do
    src <- requireCheckJsonBody :: Handler SourceFile
    case PhpParser.parseProgram (filename src) (content src) of
        Left errorMsg -> returnJson (Error "FAILED" errorMsg)
        Right ast -> returnJson ast

postFromPyR :: Handler Value
postFromPyR = do
    src <- requireCheckJsonBody :: Handler SourceFile
    case PyParser.parseProgram (filename src) (content src) of
        Left errorMsg -> returnJson (Error "FAILED" errorMsg)
        Right ast -> returnJson ast

postFromRbR :: Handler Value
postFromRbR = do
    src <- requireCheckJsonBody :: Handler SourceFile
    case RbParser.parseProgram (filename src) (content src) of
        Left errorMsg -> returnJson (Error "FAILED" errorMsg)
        Right ast -> returnJson ast

postFromJsR :: Handler Value
postFromJsR = do
    src <- requireCheckJsonBody :: Handler SourceFile
    case JsParser.parseProgram (filename src) (content src) of
        Left errorMsg -> returnJson (Error "FAILED" errorMsg)
        Right ast -> returnJson ast

postFromTsR :: Handler Value
postFromTsR = do
    src <- requireCheckJsonBody :: Handler SourceFile
    case TsParser.parseProgram (filename src) (content src) of
        Left errorMsg -> returnJson (Error "FAILED" errorMsg)
        Right ast -> returnJson ast

main :: IO ()
main = warp 3000 App
