{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DataKinds         #-}
module Api where

import Markup
import Types

import Data.Aeson
import Data.ByteString (ByteString ())
import Data.Text (Text ())
import Servant.API

data Entry'
  = Entry'
  { entryReplyTo' :: Maybe Id
  , entryImg'     :: Maybe Id
  , entryText'    :: Markup Text
  } deriving (Show, Eq, Ord)

data Entry
  = Entry
  { entryId         :: Id
  , entryReplyTo    :: Maybe Id
  , entryImg        :: Maybe Id
  , entryText       :: Markup Text
  , entryDeleteCode :: Maybe DeleteCode
  } deriving (Show, Eq, Ord)

instance FromJSON Entry' where
  parseJSON = withObject "Entry'" $ \v ->
    Entry' <$> v .: "reply_to"
           <*> v .: "img"
           <*> (parseMarkup <$> v .: "text")

instance ToJSON Entry where
  toJSON (Entry id replyTo img text _) = object
    [ "id" .= id
    , "reply_to" .= replyTo
    , "img" .= img
    , "text" .= toText text
    ]

data IdAndDeleteCode
  = IdAndDeleteCode
  { idcId :: Id
  , idcDeleteCode :: DeleteCode
  } deriving (Show, Eq, Ord)

instance ToJSON IdAndDeleteCode where
  toJSON (IdAndDeleteCode id code) = object [ "id" .= id, "delete_code" .= code ]

newtype Moderator = Moderator { unModerator :: Text }
  deriving (Show, Eq, Ord)

data BoardInfo
  = BoardInfo
  { bName        :: Name
  , bDescription :: Text
  , unlisted     :: Bool
  } deriving (Show, Eq, Ord)

instance FromJSON BoardInfo where
  parseJSON = withObject "BoardInfo" $ \v ->
    BoardInfo <$> v .: "name" <*> v .: "description" <*> v .: "unlisted"

instance ToJSON BoardInfo where
  toJSON (BoardInfo name desc unlisted) = object
    [ "name" .= name
    , "description" .= desc
    , "unlisted" .= unlisted
    ]

data Report'
  = Report'
  { reportEntryId' :: Id
  , reportText'    :: Text
  } deriving (Show, Eq, Ord)

instance FromJSON Report' where
  parseJSON = withObject "Report'" $ \v ->
    Report' <$> v .: "entry_id" <*> v .: "text"

data Report
  = Report
  { reportId      :: Id
  , reportEntryId :: Id
  , reportText    :: Text
  } deriving (Show, Eq, Ord)

instance ToJSON Report where
  toJSON (Report id entryId text) = object
    [ "id" .= id
    , "entry_id" .= entryId
    , "text" .= text
    ]

type CCChanRoutes = "api" :> CCChanApi :<|> Raw

type CCChanApi = "v1" :> CCChanApi1

type ModerationApi = "reports" :> Get '[JSON] [Report]
  :<|> "reports" :> Capture "id" Id :> Get '[JSON] Report
  :<|> "reports" :> Capture "id" Id :> "agree" :> DeleteNoContent '[JSON] NoContent
  :<|> "reports" :> Capture "id" Id :> "disagree" :> DeleteNoContent '[JSON] NoContent
  :<|> "delete_codes" :> "entries" :> Capture "id" Id :> Get '[JSON] DeleteCode
  :<|> "boards" :> ReqBody '[JSON] BoardInfo :> Post '[JSON] Name
  :<|> "boards" :> Capture "name" Name :> ReqBody '[JSON] BoardInfo :> Put '[JSON] Name
  :<|> "boards" :> Capture "name" Name :> DeleteNoContent '[JSON] NoContent

type BoardApi1 = "entries" :> Get '[JSON] [Entry]
  :<|> "entries" :> ReqBody '[JSON] Entry' :> Post '[JSON] IdAndDeleteCode
  :<|> "entries" :> Capture "id" Id :> Get '[JSON] Entry
  :<|> "entries" :> Capture "id" Id :> ReqBody '[JSON] DeleteCode :> DeleteNoContent '[JSON] NoContent

type CCChanApi1 = "boards" :> Get '[JSON] [BoardInfo]
  :<|> "boards" :> Capture "name" Name :> BoardApi1
  :<|> "images" :> Get '[JSON] [Id]
  :<|> "images" :> QueryFlag "spoiler" :>  ReqBody '[OctetStream] ByteString :> Post '[JSON] Id
  :<|> "images" :> Capture "id" Id :> Get '[OctetStream] ByteString
  :<|> "images" :> Capture "id" Id :> "spoiler" :>  Get '[JSON] Bool
  :<|> "reports" :> ReqBody '[JSON] Report' :> Post '[JSON] Id
  :<|> BasicAuth "moderation" Moderator :> ModerationApi