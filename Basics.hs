-- The basics of Haskell
-- C-c C-b: initialize haskell shell GHCi
-- C-c C-l: load current file into the shell
module Main
       where

import System.IO
import Data.Char(toUpper)
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM

x = 5
y = (6, "salut")
z = x * fst y

square x = x * x

signum x = 
       if x < 0 
         then -1
       else if x > 0
         then 1
         else 0

stair x = 
      case x of 
           0 -> 1
           1 -> 2
           2 -> 3
           _ -> 3/0

roots a b c = 
      let det  = sqrt (b*b - 4*a*c)
      in ((-b + det) / (2*a), (-b - det) / (2*a))

-- Recursion
factorial 1 = 1
factorial n = n * factorial (n-1)

-- IO
checkin :: IO ()
checkin = do
        putStrLn "Salut tout le monde! Comment vous-appelez vous?"
        votre_nom <- getLine
        putStrLn $ "Bienvenue a l'Haskell, " ++ votre_nom ++ "!\n"
    
-- File IO
fichier = do
        fin <- openFile "Basics.hs" ReadMode
        fout <- openFile "Basics.gen.hs" WriteMode
        proc_fichier fin fout
        hClose fin
        hClose fout

-- return used to wrap a pure result to IO
proc_fichier fin fout = 
        do ineof <- hIsEOF fin
           if ineof then do
              putStrLn "vous etes finis"
              return ()
           else do line <- hGetLine fin                   
                   hPutStrLn fout (map toUpper line)
                   proc_fichier fin fout

-- -- Software Transactional Memory (STM)
-- touch_stm = do shared <- atomically $ newTVar 0
--                before <- atomRead shared
--                putStrLn $ "Before: " ++ show before
--                forkIO $ 25 `timesDo` (dispVar shared >> milliSleep 20)
--                forkIO $ 10 `timesDo` (appV (+ 2) shared >> milliSleep 50)
--                forkIO $ 20 `timesDo` (appV pred shared >> milliSleep 25)
--                milliSleep 800
--                after <- atomRead shared
--                putStrLn $ "After: " ++ show after       
--                where timesDo = replicateM_
--                      milliSleep = threadDelay . (*) 1000

main = putStrLn "salut tout le monde"
