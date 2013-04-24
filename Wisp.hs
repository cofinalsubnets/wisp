module Wisp
( interpreter
, interpreter'
, repl
) where

import Wisp.Types
import Wisp.Core
import Wisp.Primitives
import Wisp.Reader
import Control.Monad
import Control.Monad.ST
import Control.Monad.Reader
import Text.ParserCombinators.Parsec (ParseError)
import System.IO
import System.Timeout

type Interpreter m s = String -> m (Either ParseError (Value s))

interpreter' :: ST s (Interpreter (ST s) s)
interpreter' = do
  tl <- mkToplevel
  let env    = Env tl (return . Str)
      terp v = runReaderT (eval v tl return) env
  return $ \s -> case parseWisp s of
    Left err  -> return $ Left err
    Right v   -> fmap return $ terp v

interpreter :: IO (Interpreter IO RealWorld)
interpreter = stToIO $ interpreter' >>= return . (stToIO .)

repl :: Handle -> Handle -> String -> Maybe Int -> IO ()
repl i o p w = do
  terp <- interpreter
  case w of
    Nothing -> forever $ iter terp
    Just t -> withTimeout t terp
  where
    iter terp = do
      hPutStr o p
      hFlush o
      l <- hGetLine i
      when (l /= "") $ do
        res <- terp l
        hPutStrLn o $ case res of
          Left e -> show e
          Right v -> show v
        hFlush o
    withTimeout t terp = do
      l <- timeout t $ iter terp
      case l of
        Nothing -> return ()
        Just _ -> withTimeout t terp
        
