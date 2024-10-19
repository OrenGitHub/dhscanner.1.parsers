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
import Data.Text

-- project imports
import qualified Ast

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

data Error = Error String String String deriving ( Generic )

-- | indicate a parse error 
instance ToJSON Error where toJSON (Error status message _filename) = object [ "status" .= status, "message" .= message, "filename" .= _filename ]

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
postFromPhpR = post PhpParser.parseProgram

postFromPyR :: Handler Value
postFromPyR = post PyParser.parseProgram

postFromRbR :: Handler Value
postFromRbR = post RbParser.parseProgram

postFromJsR :: Handler Value
postFromJsR = post JsParser.parseProgram

postFromTsR :: Handler Value
postFromTsR = post TsParser.parseProgram

postFailed :: String -> String -> Handler Value
postFailed errorMsg _filename = do
    $(logOther " INFO ") (Data.Text.pack errorMsg)
    returnJson (Error "FAILED" errorMsg _filename)

postSucceeded :: Ast.Root -> Handler Value
postSucceeded ast = do
    returnJson ast

post :: (FilePath -> String -> Either String Ast.Root) -> Handler Value
post parseProgram = do
    src <- requireCheckJsonBody :: Handler SourceFile
    case parseProgram (filename src) (content src) of
        Left errorMsg -> postFailed errorMsg (filename src)
        Right ast -> postSucceeded ast

main :: IO ()
main = warp 3000 App
