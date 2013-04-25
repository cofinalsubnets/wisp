{-# LANGUAGE RankNTypes, PatternGuards, TupleSections #-}
module Wisp.Primitives (mkToplevel) where

import Wisp.Types
import Wisp.Predicates
import Wisp.Core
import Wisp.Reader
import Wisp.STL
import Control.Monad
import Control.Monad.ST
import Control.Monad.Reader
import Control.Monad.Writer
import Data.HashTable.Class (fromList)
import System.Random

mkToplevel :: ST s (Frame s)
mkToplevel = do
  base <- return . F Nothing =<< bs
  _ <- runWriterT $ runReaderT (eval stl base return) $ Env base (error . show) "" (mkStdGen 0)
  return base
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
      , (pack "read",       p_read)
      , (pack "print",      p_print)
      , (pack "rand",       p_rand)
      , (pack "mod",        p_mod)
      , (pack "get-line",   p_get_line)
      , (pack "get-char",   p_get_char)
      , (pack "get-input",  p_get_input)
      ]

ylppa = flip ($)

-- PRIMITIVE FN COMBINATORS
-- | Predicate wrapper for variadic typechecking functions.
check p = Prim (anyNumber arguments) $ ylppa . Bln . all p

-- PRIMITIVE FUNCTIONS

-- | string coercion
p_str = Prim (anyNumber arguments) $ ylppa . Str . concatMap stringify
  where
    stringify (Str s) = s
    stringify v = show v

-- | call-with-current-continuation
p_call_cc = Prim (Exactly 1 [applicable]) $ \[a] c -> apply a [cc c] c
  where
    cc c = Prim (Exactly 1 arguments) $ \[v] _ -> c v

-- | equality
p_eq = Prim (anyNumber arguments)
     $ \vs -> ylppa . Bln . and . zipWith (==) vs $ drop 1 vs

-- | apply
p_apply = Prim (Exactly 2 [applicable, list]) $ \[a, Lst l] -> apply a l

p_eval = Prim (Exactly 1 arguments) $ \[v] c -> do
  tl <- asks toplevel
  eval v tl c

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
  where
    splat = Sym $ pack "&"

-- | string -> symbol coercion
p_sym = Prim (Exactly 1 strings) $ \[Str s] -> ylppa (Sym $ pack s)

-- | division
p_div = Prim (AtLeast 1 numbers) $ \(h:t) c -> case foldM s_div h t of
  Left err -> wispErr err
  Right v -> c v

-- | Parse a string into wisp data.
p_read = Prim (Exactly 1 strings) $ \[Str s] c -> case parseWisp s of
  Left err -> wispErr $ show err
  Right v -> c v



-- Sandboxed basic IO operations via reader/writer monads.

io_get :: (String -> Maybe (String, String)) -> Value s
io_get fn = Prim (Exactly 0 arguments) $ \_ c -> do
  i <- asks input
  case fn i of
    Just (k,ks) -> local (\e -> e{input=ks}) $ c (Str k)
    _ -> wispErr "ERROR: end of input"

p_get_line = io_get $ \i ->
  if null i then Nothing
  else let (l,ls) = break (=='\n') i in
    return (l, drop 1 ls)

p_get_char = io_get $ \i ->
  if null i then Nothing
  else return ([head i], tail i)

p_get_input = io_get $ return . (,"")

p_print = Prim (Exactly 1 strings) $ \[str@(Str s)] c -> do
  tell s
  c str


-- Math operations.

-- | Wrapper for variadic math operations on arbitrary numeric types
math :: (forall a. Num a => a -> a -> a) -> Value s
math op = Prim (AtLeast 1 numbers) $ ylppa . foldl1 (s_num_op op)

-- | Polymorphic binary math op application. Handles coercion between numeric
-- types.
s_num_op :: (forall a. Num a => a -> a -> a) -> Value s -> Value s -> Value s
s_num_op (?) s1 s2 = case (s1,s2) of
  (Int a, Int b) -> Int $ a ? b
  (Int a, Flt b) -> Flt $ fromIntegral a ? b
  (Flt a, Int b) -> Flt $ a ? fromIntegral b
  (Flt a, Flt b) -> Flt $ a ? b

-- | Division within & across numeric types.
s_div :: Value s -> Value s -> Either String (Value s)
s_div s1 s2
 | s2 == Int 0 || s2 == Flt 0 = Left "ERROR: divide by zero"
 | otherwise = return $ case (s1,s2) of
   (Int a, Int b) -> Int $ quot a b
   (Int a, Flt b) -> Flt $ fromIntegral a / b
   (Flt a, Int b) -> Flt $ a / fromIntegral b
   (Flt a, Flt b) -> Flt $ a / b

p_mod = Prim (Exactly 2 integers) $ \[Int a, Int b] ->
  ylppa . Int $ a `mod` b

-- | Comparison.
p_lt = Prim (Exactly 2 numbers) $ \ns -> ylppa $ Bln $ case ns of
  [Int a,  Int b] -> a < b
  [Int a,  Flt b] -> fromIntegral a < b
  [Flt a, Int b]  -> a < fromIntegral b
  [Flt a, Flt b]  -> a < b

p_rand = Prim (Exactly 0 arguments) $ \_ c -> do
  gen <- asks randomSeed
  let (i,ng) = random gen
  local (\e -> e{randomSeed=ng}) $ c $ Int i
 
