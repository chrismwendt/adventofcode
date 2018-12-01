{-# LANGUAGE OverloadedStrings #-}

module Main where

main :: IO ()
main = do
  putStr "hello, world\n"

  return ()

ghci :: IO ()
ghci = main