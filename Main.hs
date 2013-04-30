module Main where

import Wisp
import System.Console.GetOpt
import System.Environment
import System.Exit

options :: [OptDescr (IO ())]
options = [ Option "" ["repl"]
            (NoArg repl)
            "start wisp REPL"
          ]

main :: IO ()
main = do
  opts <- getArgs >>= return . getOpt Permute options 
  case opts of
    (os,fs,[]) -> do
      sequence_ os
      terp <- interpreter
      progs <- mapM readFile fs
      outs <- mapM (terp . ("(do "++) . (++")")) progs
      mapM_ putStr outs
    _ -> exitFailure

repl :: IO ()
repl = interpreter >>= loop
  where
    loop i = getLine >>= i >>= putStrLn >> loop i

  

