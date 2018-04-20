{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}


module Main where

import Lib


import qualified Language.C.Inline as C

C.include "<math.h>"
C.include "test.c"

main :: IO ()
main = do
  x <- [C.exp| double{ cos(1) } |]
  y <- [C.exp| double{ 1.47 } |]
  z <- [C.exp| int{ testFunc(2) } |]
  print x
  print y
  print z