{-# OPTIONS -Wall #-}

module Main where

import Control.Monad.State

type Info = (Int, Int)

getRow :: State () Info -> Info
getRow s = fst $ runState s ()

test :: State () Info
test = do
   return (3,3)

info :: Info
info = fst $ runState test ()


setInfo :: Info -> State Info ()
setInfo i = put i
  
getInfo :: State Info Info
getInfo = do
  i <- get
  put i
  return i
  
foo :: State () Info
foo = state $ \() -> ((2,2), ())

se