{-# LANGUAGE TupleSections #-}
module Wisp.Core (eval, apply) where

import Wisp.Types
import Wisp.Predicates
import qualified Data.HashTable.ST.Cuckoo as H
import Data.HashTable.Class (fromList)
import Control.Monad
import Control.Monad.Reader


eval :: Value s -- the thing being evaluated
     -> Frame s -- the evaluation context
     -> Continue s

eval (Sym s) f c = findBinding s f >>= maybe nameError (c . fst)
  where
    nameError = wispErr $ "ERROR: unable to resolve symbol: " ++ unpack s

eval (Lst (SF m:ps)) f c = specialForm m ps f c

eval (Lst (o:ps)) f c = eval o f go
  where
    go fn = if macro fn then apply fn ps $ \r -> eval r f c
            else evalArgs fn ps []

    evalArgs fn []     es = apply fn (reverse es) c
    evalArgs fn (a:as) es = eval a f $ evalArgs fn as . (:es)

eval v _ c = c v



apply :: Value s   -- the value being applied
      -> [Value s] -- the arguments
      -> Continue s

apply p@Prim{} []
 | satisfied p = call p []
 | otherwise = ($p)

apply p@Prim{argSpec = spec} (a:as)
 | spec `admits` a = Prim admit (call p . (a:)) `apply` as
 | otherwise = const $ wispErr $ "ERROR: bad type: " ++ show a
 where
   admit = spec {count = max 0 (pred $ count spec), guards = drop 1 $ guards spec}

apply fn@Fn{} as = either (const . wispErr) funcall $ destructure (Lst bound) (Lst as)
  where
    funcall kvs c = do
      f <- wispST (fromList kvs) >>= return . F (Just $ closure fn)
      let fn' = fn{params = unbound, closure = f}
      if satisfied fn' then eval (body fn') f c else c fn'

    (bound, unbound) = if length as >= nPos then (params fn, [])
                       else splitAt (length as) (params fn)
    nPos = length . fst . posVarArgs $ params fn

apply v _ = const . wispErr $ "ERROR: can't apply value: " ++ show v


-- helper fns

-- | Traverse a binding pattern together with a set of values, an return
-- a list of bindings or an error.
destructure :: Value s -- the binding pattern
            -> Value s -- the values to be bound
            -> Either String [(Symbol, Value s)] -- an error or a list of bindings

destructure (Sym s) v = Right [(s,v)]

destructure l0@(Lst l) v0@(Lst v) 
 | (req, Just s) <- posVarArgs l = do
   let (pn,vn) = splitAt (length req) v
   pos <- destructure (Lst req) (Lst pn)
   var <- destructure s (Lst vn)
   return $ var ++ pos
 | length l == length v = fmap concat . sequence $ zipWith destructure l v
 | otherwise = structError l0 v0

destructure p v = structError p v

structError p v = Left . unwords $
  [ "ERROR: structure mismatch in pattern:" , show p , "<-" , show v ]


-- | Search for a binding visible from a frame. Return the value and the
-- frame in which it is bound, or Nothing.
findBinding :: Symbol -> Frame s -> Wisp s (Maybe (Value s, Frame s))
findBinding nm f = wispST (H.lookup (bindings f) nm)
               >>= maybe iter (return . return . (,f))
  where iter = maybe (return Nothing) (findBinding nm) (parent f)


-- | Return the positional & variadic parameters of a parameter list.
-- If multiple variadic parameters are supplied, ignores all but the first
-- one.
posVarArgs :: [Value s] -> ([Value s], Maybe (Value s))
posVarArgs p = case break (== Sym (pack "&")) p of
  (ps,_:v:_) -> (ps, Just v)
  _          -> (p, Nothing)


-- | Special form handler.
specialForm :: Form       -- the special form
            -> [Value s]  -- the arguments
            -> Frame s    -- the calling context
            -> Continue s -- continuation

specialForm Do (p:ps) f c = eval p f $
  if null ps then c else const (specialForm Do ps f c)

specialForm If [cond,y,n] f c = eval cond f $ \res ->
  eval (if res == Bln False then n else y) f c

specialForm Lambda (Lst ps:b) f c = c $ Fn ps False (Lst $ SF Do:b) f
specialForm Macro  (Lst ps:b) f c = c $ Fn ps True  (Lst $ SF Do:b) f

specialForm Quote [v] _ c = c v

specialForm Quasiquote [val] f cont = spliceV val cont
  where
    spliceV (Lst [SF Splice, v]) = eval v f
    spliceV (Lst l) = spliceL l []
    spliceV v = ($v)
    spliceL [] l' c = c $ Lst l'
    spliceL (Lst l:t) vs c
     | [SF Merge, v] <- l = eval v f $ \v' -> case v' of
       Lst l' -> spliceL t (vs ++ l') c
       _ -> wispErr $ "ERROR: can't merge non-list: " ++ show v'
     | otherwise = spliceV (Lst l) $ \l' -> spliceL t (vs ++ [l']) c
    spliceL (v:t) vs c = spliceL t (vs ++ [v]) c

specialForm Splice _ _ _ = wispErr "ERROR: splice outside quasiquoted expression"
specialForm Merge _ _ _ = wispErr "ERROR: merge outside quasiquoted expression"

specialForm Def [s, xp] f c = eval xp f $ \v ->
  either wispErr (def v) $ destructure s v
  where
    bindV = uncurry $ H.insert $ bindings f
    def v kvs = do
      wispST $ mapM_ bindV kvs
      c v

specialForm Set [Sym s, xp] f c = findBinding s f >>= maybe nameError setV
  where
    nameError = wispErr $ "ERROR: set: free or immutable variable: " ++ unpack s
    setV (_, tf) = eval xp f $ \v -> do
      wispST $ H.insert (bindings tf) s v
      c v

specialForm Undef [Sym s] f c = findBinding s f >>= maybe nameError unbind
  where
    nameError = wispErr $ "ERROR: undef: free or immutable variable: " ++ unpack s
    unbind (v, tf) = do
      wispST $ H.delete (bindings tf) s
      c v

specialForm Catch (handler:xps) f c = eval handler f $ \h ->
  let h' e = apply h [Str e] c in
  local (\e -> e{abort=h'}) $ eval (Lst $ SF Do:xps) f c

specialForm sf _ _ _ = wispErr $ "ERROR: syntax error in special form: " ++ show sf

