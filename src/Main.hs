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
import Text.Read (readMaybe)
import Data.Time
import Yesod.Core.Types
import System.Log.FastLogger
import Network.Wai.Handler.Warp

-- Wai stuff
import qualified Network.Wai
import qualified Network.Wai.Logger
import qualified Network.HTTP.Types.Status
import qualified Network.Wai.Middleware.RequestLogger as Wai

-- project imports
import qualified Location
import qualified Ast

-- project imports
import qualified JsParser
import qualified CsParser
import qualified GoParser
import qualified TsParser
import qualified PyParser
import qualified RbParser
import qualified PhpParser

data SourceFile
   = SourceFile
     {
         filename :: String,
         content :: String,
         module_name_resolver :: String
     }
     deriving ( Generic, ToJSON, FromJSON )

data Healthy = Healthy Bool deriving ( Generic )

data Error
   = Error
     {
         status :: String,
         location :: Location.Location
     }
     deriving ( Generic, ToJSON )

-- | This is just for the health check ...
instance ToJSON Healthy where toJSON (Healthy status) = object [ "healthy" .= status ]

data App = App

mkYesod "App" [parseRoutes|
/from/php/to/dhscanner/ast FromPhpR POST
/from/py/to/dhscanner/ast FromPyR POST
/from/rb/to/dhscanner/ast FromRbR POST
/from/js/to/dhscanner/ast FromJsR POST
/from/cs/to/dhscanner/ast FromCsR POST
/from/go/to/dhscanner/ast FromGoR POST
/from/ts/to/dhscanner/ast FromTsR POST
/healthcheck HealthcheckR GET
|]

instance Yesod App where
    makeLogger = \_app -> myLogger
    maximumContentLength = \_app -> (\_anyRouteReally -> Just 80000000)

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

postFromCsR :: Handler Value
postFromCsR = post CsParser.parseProgram

postFromGoR :: Handler Value
postFromGoR = post GoParser.parseProgram

postFromTsR :: Handler Value
postFromTsR = post TsParser.parseProgram

postFailed :: String -> String -> Handler Value
postFailed errorMsg _filename = do
    $logInfoS "(Parser)" (Data.Text.pack errorMsg)
    let defaultLoc = Location.Location _filename 1 1 1 1
    let loc = case (readMaybe errorMsg :: Maybe Location.Location) of { Just l -> l; _ -> defaultLoc }
    returnJson (Error "FAILED" loc)

postSucceeded :: Ast.Root -> Handler Value
postSucceeded = returnJson

post :: (FilePath -> String -> Either String Ast.Root) -> Handler Value
post parseProgram = do
    src <- requireCheckJsonBody :: Handler SourceFile
    case parseProgram (filename src) (content src) of
        Left errorMsg -> postFailed errorMsg (filename src)
        Right ast -> postSucceeded ast

myLogger :: IO Logger
myLogger = do
    _loggerSet <- newStdoutLoggerSet defaultBufSize
    formatter <- newTimeCache "[%d/%m/%Y ( %H:%M:%S )]"
    return $ Logger _loggerSet formatter

dateFormatter :: String -> String
dateFormatter date = let
    date' = parseTimeOrError True defaultTimeLocale "%d/%b/%Y:%T %Z" date :: UTCTime
    in formatTime defaultTimeLocale "[%d/%m/%Y ( %H:%M:%S )]" date'

unquote :: String -> String
unquote s = let n = Prelude.length s in Prelude.take (n-2) (Prelude.drop 1 s)

logify :: String -> Network.Wai.Request -> String
logify date req = let
    datePart = dateFormatter date
    method = unquote (show (Network.Wai.requestMethod req))
    url = unquote (show (Network.Wai.rawPathInfo req))
    in datePart ++ " [Info#(Wai)] " ++ method ++ " " ++ url ++ "\n"

formatter :: Network.Wai.Logger.ZonedDate -> Network.Wai.Request -> Network.HTTP.Types.Status.Status -> Maybe Integer -> LogStr
formatter zonedDate req status responseSize = toLogStr (logify (unquote (show zonedDate)) req)

loggerSettings :: Wai.RequestLoggerSettings
loggerSettings = Wai.defaultRequestLoggerSettings { Wai.outputFormat = Wai.CustomOutputFormat formatter }

main :: IO ()
main = do
    waiApp <- toWaiAppPlain App
    myLoggingMiddleware <- Wai.mkRequestLogger loggerSettings
    let middleware = myLoggingMiddleware . defaultMiddlewaresNoLogging
    run 3000 $ middleware waiApp

