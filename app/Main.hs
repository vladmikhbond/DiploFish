module Main where

import Lib (mainF, mainR)

import System.Environment (getArgs)

main :: IO ()
main = do
   args <- getArgs
   let n = length args
   if n == 0 || n > 2 
   then print "> Fish.exe <surname> [--r]"
   else if n == 1
   then 
      mainF (head args) >> print ("Ready. Feedback in " ++ head args ++ "-F.txt")
   else 
      mainR (head args) >> print ("Ready. Review in " ++ head args ++ "-R.txt")                    