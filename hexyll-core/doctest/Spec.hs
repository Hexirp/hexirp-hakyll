module Main where

  import Prelude
  import Test.DocTest (doctest)

  main :: IO ()
  main = doctest
    [ "-isrc"
    , "src/Data/List/Hexyll.hs"
    ]
