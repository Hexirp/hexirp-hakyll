module Main where

  import Prelude
  import Test.DocTest (doctest)

  main :: IO ()
  main = doctest
    [ "-isrc"
    , "src/Control/Monad/Hexyll.hs"
    , "src/Data/List/Hexyll.hs"
    , "src/Data/Yaml/Hexyll.hs"
    , "src/Hexyll/Core/Configuration.hs"
    , "src/System/Directory/Hexyll.hs"
    ]
