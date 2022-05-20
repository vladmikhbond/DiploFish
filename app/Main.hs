module Main where

import Lib (run)

import System.Environment (getArgs)

main :: IO ()
main = do
   args <- getArgs
   let n = length args
   let surname = head args 
   if n == 0 || n > 2 
   then print "> Fish.exe <surname> [R]"
   else if n == 1
   then 
      run surname 'F' >> print ("Ready. Feedback in " ++ surname ++ "-feedback.txt")
   else 
      run surname 'R' >> print ("Ready. Review in " ++ surname ++ "-review.txt")    
                 