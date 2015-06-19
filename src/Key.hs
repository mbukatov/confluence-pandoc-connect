{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Key where

import           Data.Text
import           GHC.Generics

newtype Key a = Key Text
  deriving (Show, Eq, Generic)
