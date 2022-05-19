module Main where

import Lib (mainF)

import System.Environment (getArgs)

main :: IO ()
main = do
   args <- getArgs
   if null args
      then print "> main <surname>"
      else mainF (head args) >> print ("Ready. Feedback in " ++ head args ++ "-F.txt")
                  