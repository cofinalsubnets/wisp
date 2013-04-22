{-# LANGUAGE RankNTypes, PatternGuards #-}
module Wisp.Primitives (newEnv) where

import Prelude hiding (lookup)
import Wisp.Types
import Wisp.Predicates
import Wisp.Core
import Wisp.Reader
import Wisp.STL
import Control.Monad
import Control.Monad.RWS.Strict
import Data.HashTable.Class (fromList)
import qualified Data.HashTable.ST.Cuckoo as H
import System.IO

newEnv = do
  base <- return . F Nothing =<< bs
  case parseWisp stl of
    Left err -> error $ show err
    Right t -> do
      let env = Env "" error
      fmap fst $ execRWST (eval t base return) env base
  where
    bs = fromList $
      [ (pack "+",          math (+))
      , (pack "-",          math (-))
      , (pack "*",          math (*))
      , (pack "/",          p_div)
      , (pack "=",          p_eq)
      , (pack "apply",      p_apply)
      , (pack "eval",       p_eval)
      , (pack "str",        p_str)
      , (pack "symbol",     p_sym)
      , (pack "int",        p_int)
      , (pack "error",      p_err)
      , (pack "arity",      p_arity)
      , (pack "bool?",      check bool  )
      , (pack "integer?",   check integer)
      , (pack "string?",    check string   )
      , (pack "number?",    check number   )
      , (pack "func?",      check function  )
      , (pack "list?",      check list  )
      , (pack "symbol?",    check symbol   )
      , (pack "primitive?", check primitive  )
      , (pack "macro?",     check macro )
      , (pack "<",          p_lt)
      , (pack "call/cc",    p_call_cc)
      , (pack "get-line",   p_get_line)
      , (pack "print",      p_print)
      , (pack "read",       p_read)
      ]

ylppa = flip ($)

-- PRIMITIVE FN COMBINATORS
-- | variadic math operations. division is handled as a special case.
math :: (forall a. Num a => a -> a -> a) -> Value s
math op = Prim (AtLeast 1 numbers) $ ylppa . foldl1 (s_num_op op)

-- | predicate wrapper for variadic typechecking
check p = Prim (anyNumber arguments) $ ylppa . Bln . all p

-- PRIMITIVE FUNCTIONS

-- | string coercion
p_str = Prim (anyNumber arguments) $ ylppa . Str . concatMap stringify
  where
    stringify (Str s) = s
    stringify v = show v

p_call_cc = Prim (Exactly 1 [applicable]) $ \[a] c -> apply a [cc c] c
  where cc c = Prim (Exactly 1 arguments) $ \[v] _ -> c v

-- | numeric comparison
p_lt = Prim (Exactly 2 numbers) $ \ns -> ylppa $ Bln $ case ns of
  [Int a,  Int b] -> a < b
  [Int a,  Flt b] -> fromIntegral a < b
  [Flt a, Int b]  -> a < fromIntegral b
  [Flt a, Flt b]  -> a < b

-- | equality
p_eq = Prim (anyNumber arguments)
     $ \vs -> ylppa . Bln . and . zipWith (==) vs $ drop 1 vs

-- | apply
p_apply = Prim (Exactly 2 [applicable, list]) $ \[a, Lst l] -> apply a l

p_eval = Prim (Exactly 1 arguments) $ \[v] c -> RWST $ \i tl -> 
  runRWST (eval v tl c) i tl

-- | integer coercion
p_int = Prim (Exactly 1 numbers) $ ylppa . intg
  where
    intg [Flt f] = Int $ floor f
    intg [n] = n

-- | raise an error
p_err = Prim (Exactly 1 strings) $ \[Str e] _ -> wispErr $ "ERROR: " ++ e

-- | get fn arity
p_arity = Prim (Exactly 1 [function ||| macro])
        $ ylppa . Int . fromIntegral . length . takeWhile (/= splat) . params . head
  where splat = Sym $ pack "&"

-- | string -> symbol coercion
p_sym = Prim (Exactly 1 strings) $ \[Str s] -> ylppa (Sym $ pack s)

-- | division
p_div = Prim (AtLeast 1 numbers) $ \(h:t) c -> case foldM s_div h t of
  Left err -> wispErr err
  Right v -> c v

p_get_line = Prim (Exactly 0 arguments) $ \_ c -> do
  e <- ask
  case break (=='\n') $ input e of
    (l, _:i') -> local (const $ e{input = i'}) (c $ Str l)
    _ -> wispErr $ "ERROR: get-line: end of input"

p_print = Prim (Exactly 1 strings) $ \[Str s] c -> do
  tell s
  c $ Str s

p_read = Prim (Exactly 1 strings) $ \[Str s] c -> case parseWisp s of
  Left err -> wispErr $ show err
  Right v -> c v
 

-- Primitive fns and special forms

-- | polymorphic binary math op application. handles coercion between numeric
-- types
s_num_op :: (forall a. Num a => a -> a -> a) -> Value s -> Value s -> Value s
s_num_op (?) s1 s2 = case (s1,s2) of
  (Int a, Int b) -> Int $ a ? b
  (Int a, Flt b) -> Flt $ fromIntegral a ? b
  (Flt a, Int b) -> Flt $ a ? fromIntegral b
  (Flt a, Flt b) -> Flt $ a ? b

-- stop handling this as a gross special case maybe?
s_div :: Value s -> Value s -> Either String (Value s)
s_div s1 s2
 | s2 == Int 0 || s2 == Flt 0 = Left "ERROR: divide by zero"
 | otherwise = return $ case (s1,s2) of
   (Int a, Int b) -> Int $ a `quot` b
   (Int a, Flt b) -> Flt $ fromIntegral a / b
   (Flt a, Int b) -> Flt $ a / fromIntegral b
   (Flt a, Flt b) -> Flt $ a / b

