{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module ConfluenceTypes where

import           Data.Aeson
import           Data.Char        as C
import qualified Data.Text        as T
import           GHC.Generics
import           Key

data PageDetails = PageDetails
  { pageType  :: PageType
  , pageTitle :: T.Text
  , pageSpace :: Space
  , pageBody  :: Body
  , pageAncestors :: [PageId]
  }

newtype PageId = PageId Integer
  deriving (Generic, Eq, Show)

data PageType = Page | Blogpost
  deriving (Generic, Eq, Enum, Show)

newtype Space = Space (Key Space)
  deriving (Generic, Eq, Show)
instance ToJSON Space where
  toJSON (ConfluenceTypes.Space (Key k))= object ["key" .= toJSON k]

newtype Body = Body T.Text
  deriving (Generic, Eq, Show)

instance ToJSON PageDetails where
  toJSON pd = object
    [ "type" .= (map toLower . show $ pageType pd)
    , "title" .= toJSON (pageTitle pd)
    , "space" .= toJSON (pageSpace pd)
    , "body" .= object
        [ "storage" .= object
          [ "value" .= (\(Body t) -> toJSON t) (pageBody pd)
          , "representation" .= String "storage"
          ]
        ]
    --, "ancestors" .= toJSON (map (\(PageId i) -> object ["type" .= String "page", "id" .= Number (fromInteger i)]) $ pageAncestors pd)
    ]
