{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Questionnaire where

import GHC.Generics
import Data.Aeson

data Section = Section { header :: String
                       , items :: [String]
                       } deriving (Show, Generic)

instance ToJSON Section
instance FromJSON Section

data Questionnaire = Questionnaire { name :: String
                                   , sections :: [Section]
                                   } deriving (Show, Generic)

instance ToJSON Questionnaire
instance FromJSON Questionnaire

