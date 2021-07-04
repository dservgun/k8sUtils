module Main where

import Config.ExternalDBConfiguration

main :: IO ()
main = do

  dbParams <- loadExternalDBParameters "./configurations/externalDBParameters.config"
  print dbParams
  
