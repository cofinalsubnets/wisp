{-# LANGUAGE RankNTypes #-}
module Wisp where

import Wisp.Types
import Wisp.Core
import Wisp.Primitives
import Wisp.Reader
import Control.Monad.ST
import Control.Monad.RWS.Strict

runWisp :: String -> String -> String
runWisp prog input = output
  where
    output = runST $ do
      tl <- newEnv
      case parseWisp prog of
        Left err -> return $ show err
        Right v -> fmap snd $
          evalRWST (eval v tl return) (Env input (return . Str)) tl


