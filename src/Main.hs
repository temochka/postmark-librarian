{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Control.Monad
import Network.HTTP.Simple
import Network.HTTP.Client.TLS
import System.Environment
import System.Exit
import System.FilePath.Posix (FilePath, (</>))
import System.Console.GetOpt
import System.IO.Unsafe
import Data.Aeson
import Data.ByteString.Char8 (pack)
import Data.Maybe (fromMaybe)
import Data.Semigroup ((<>))
import Data.Time.Calendar
import Data.Time.Format


import qualified Network.HTTP.Conduit as Conduit (Manager)
import qualified Data.ByteString as S (ByteString)
import qualified Data.ByteString.Lazy as LS (writeFile)

(|>) = flip ($)

data Settings =
  Settings
    { fromDate :: Maybe Day
    , toDate :: Maybe Day
    , serverToken :: String
    , limit :: Int
    , targetDir :: FilePath
    , apiEndpoint :: String
    } deriving Show

emptySettings :: String -> Settings
emptySettings token =
  Settings
    { fromDate = Nothing
    , toDate = Nothing
    , serverToken = token
    , limit = 100
    , targetDir = "."
    , apiEndpoint = "api.postmarkapp.com"
    }

data Flag
  = ServerToken String
  | TargetDir FilePath
  | FromDate Day
  | ToDate Day
  | Limit Int
  deriving Show

data Message =
  Message
    { messageId :: String
    , subject :: String
    , from :: String
    , textBody :: Maybe String
    , htmlBody :: Maybe String
    } deriving Show

instance FromJSON Message where
  parseJSON (Object json) =
    Message <$>
      json .: "MessageID" <*>
      json .: "Subject" <*>
      json .: "From" <*>
      json .:? "TextBody" <*>
      json .:? "HtmlBody"

instance ToJSON Message where
  toJSON (Message { messageId, subject, from, textBody, htmlBody}) =
    object
      [ "MessageID" .= messageId
      , "Subject" .= subject
      , "From" .= from
      , "TextBody" .= textBody
      , "HtmlBody" .= htmlBody
      ]

data MessagesResponse =
  MessagesResponse Int [Message]
  deriving Show

instance FromJSON MessagesResponse where
  parseJSON (Object json) =
    MessagesResponse <$>
      json .: "TotalCount" <*>
      json .: "Messages"

supportedOptions :: [OptDescr Flag]
supportedOptions =
  [ Option ['l'] ["limit"] (ReqArg (Limit . read) "n") "Limit the maximum number of loaded messages (default 100)"
  , Option ['t'] ["target-dir"] (ReqArg TargetDir "TARGET_DIR") "The directory where to put downloaded messages (defaults to current dir)"
  , Option [] ["from"] (ReqArg (FromDate . readDate) "YYYY-MM-DD") "Start date"
  , Option [] ["to"] (ReqArg (ToDate . readDate) "YYYY-MM-DD") "End date"
  ]
  where
    readDate = parseTimeOrError False defaultTimeLocale "%Y-%m-%d"

setFlag :: Flag -> Settings -> Settings
setFlag flag settings = case flag of
  TargetDir dir ->
    settings { targetDir = dir }
  Limit limit ->
    settings { limit = limit }
  FromDate day ->
    settings { fromDate = Just day }
  ToDate day ->
    settings { toDate = Just day }

parseArgs :: [String] -> IO Settings
parseArgs argv =
  case getOpt Permute supportedOptions argv of
    (options, [token], []) ->
      return $ foldr setFlag (emptySettings token) options
    (_, _, errs) -> do
      putStrLn (concat errs ++ usageInfo header supportedOptions)
      exitWith (ExitFailure 1)
  where
    header = "Usage: librarian [options] <api_token>"

postmarkApiRequest :: Conduit.Manager -> String -> String -> Request
postmarkApiRequest manager endpoint token =
  defaultRequest
    |> setRequestHost (pack endpoint)
    |> setRequestManager manager
    |> setRequestHeader "Accept" ["application/json"]
    |> setRequestHeader "Content-Type" ["application/json"]
    |> setRequestHeader "X-Postmark-Server-Token" [pack token]

getMessages :: Request -> Int -> Int -> Maybe Day -> Maybe Day -> IO MessagesResponse
getMessages request offset perPage fromDate toDate =
  liftM getResponseBody $ httpJSON request'
  where
    request' =
      request |> setRequestMethod "GET"
              |> setRequestPath "/messages/outbound"
              |> setRequestQueryString
                  [ ("count", Just $ (pack . show) perPage)
                  , ("offset", Just $ (pack . show) offset)
                  , ("fromdate", fmap (pack . show) fromDate)
                  , ("todate", fmap (pack . show) toDate)
                  ]

getFullMessage :: Request -> Message -> IO Message
getFullMessage request message = do
  putStrLn $ "Loading message with ID: " ++ (messageId message)
  liftM getResponseBody $ httpJSON request'
  where
    request' =
      request |> setRequestMethod "GET"
              |> (setRequestPath $ pack $ "/messages/outbound/" ++ messageId message ++ "/details")

saveMessage :: FilePath -> Message -> IO ()
saveMessage targetDir message = do
  let fileName = targetDir </> (messageId message ++ ".json")
  let divider = take 80 $ repeat '-'
  putStrLn $ "Subject: " ++ subject message
  putStrLn $ "MessageID: " ++ messageId message
  putStrLn $ "Saving to " ++ fileName
  putStrLn divider
  LS.writeFile fileName (encode message)

messages :: Request -> Int -> Int -> Maybe Day -> Maybe Day -> IO [Message]
messages request offset batchSize fromDate toDate = do
  MessagesResponse total page <- getMessages request offset batchSize fromDate toDate
  let nextRequest | offset + batchSize < total = messages request (offset + batchSize) batchSize fromDate toDate
                  | otherwise = return []
  liftM (page ++) $ unsafeInterleaveIO nextRequest

main :: IO ()
main = do
  settings <- getArgs >>= parseArgs
  manager <- newTlsManager

  let request = (postmarkApiRequest manager (apiEndpoint settings) (serverToken settings))

  (mapM_ $ \m -> (getFullMessage request m) >>= (saveMessage (targetDir settings))) <$>
    (take (limit settings)) <$>
    messages request 0 10 (fromDate settings) (toDate settings)

  exitWith ExitSuccess
