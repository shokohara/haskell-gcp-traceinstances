module Option where

data Option =  Option {
                      serverPort :: Int
                      , clientHost :: String
                      , clientPort :: Int
                      }
