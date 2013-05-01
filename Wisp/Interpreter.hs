-- simple embedded wisp interpreters
module Wisp.Interpreter
( interpreter
, interpreter'
) where

import Wisp.Types
import Wisp.Core
import Wisp.Primitives
import Wisp.Reader
import Control.Monad.ST
import Control.Monad.Reader
import Control.Monad.Writer
import System.Random

type Interpreter m = String -> m String


interpreter :: IO (Interpreter IO)
interpreter = newStdGen >>= stToIO . interpreter' >>= return . (stToIO.)


interpreter' :: StdGen -> ST s (Interpreter (ST s))
interpreter' gen = do
  tl <- mkToplevel
  let env = Env tl reportError mempty gen
      Right val = parseWisp "(fn (s) (print (str (eval (read s)))))"

  (rep,_) <- runWriterT $ runReaderT (eval val tl return) env

  return $ \v ->
    fmap (show . snd) . runWriterT $
    runReaderT (apply rep [Str v] return) env

  where
    reportError s = do
      tell $ output s
      return $ Str s


