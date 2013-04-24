{-# LANGUAGE TupleSections #-}
module Wisp.Core
( eval
, apply
, lookup
) where

import Wisp.Types
import Wisp.Predicates
import Prelude hiding (lookup)
import qualified Data.HashTable.ST.Cuckoo as H
import Data.HashTable.Class (fromList)
import Control.Monad
import Control.Monad.ST
import Control.Monad.RWS


eval :: Value s -- the thing being evaluated
     -> Frame s -- the evaluation context
     -> Continue s
eval v f cont = case v of
 Sym s -> cont =<< lookup s f
 Lst ((SF m):ps) -> specialForm m ps f cont
 Lst (o:ps) -> eval o f $ \o' ->
   if macro o' then apply o' ps $ \r -> eval r f cont
   else let evalArgs []     ps' = apply o' (reverse ps') cont
            evalArgs (a:as) ps' = eval a f $ \a' -> evalArgs as (a':ps')
        in evalArgs ps []
 _ -> cont v


apply :: Value s   -- the value being applied
      -> [Value s] -- the arguments
      -> Continue s
apply proc args cont
 | primitive proc = foldM apC proc args >>= \p -> invoke p cont
 | applicable proc = case destructure (Lst bound) (Lst args) of
   Left err -> wispErr err
   Right kvs -> do
     f <- mkFrame (Just $ closure proc) kvs
     invoke proc{params = unbound, closure = f} cont
 | otherwise = wispErr $ "Non-applicable value: " ++ show proc

 where
  (bound, unbound) = if length args >= nPos then (params proc,[])
                     else splitAt (length args) (params proc)
  nPos = length . fst . posVarArgs $ params proc

  invoke pr
   | satisfied pr = case pr of
     Prim{} -> call pr []
     Fn{closure = cl, body = b} -> eval b cl
   | otherwise = ($pr)

  Prim as p `apC` arg
   | as `admits` arg = return $ Prim nextSpec (p . (arg:))
   | otherwise = wispErr $ "Bad type: " ++ show arg
   where
     nextSpec = as{count = max 0 (pred $ count as), guards = drop 1 $ guards as}

  mkFrame p = wispST . fromList >=> return . F p


-- | Name resolution.
lookup :: Symbol -> Frame s -> Wisp s (Value s)
lookup k = maybe nameError (return . fst) <=< findBinding k
  where nameError = wispErr $ "Unable to resolve symbol: " ++ unpack k


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
   fmap (++pos) $ destructure s (Lst vn)
 | length l == length v = fmap concat . sequence $ zipWith destructure l v
 | otherwise = structError l0 v0
destructure p v = structError p v

structError p v = Left . unwords $
  [ "Pattern error: structure mismatch:" , show p , "<-" , show v ]


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
  (ps,_)     -> (ps, Nothing)


-- SPECIAL FORMS

specialForm :: Form       -- the special form
            -> [Value s]  -- the arguments
            -> Frame s    -- the calling context
            -> Continue s

specialForm Do [p] f c = eval p f c
specialForm Do (p:ps) f c = eval p f $ \_ -> specialForm Do ps f c

specialForm If [cond,y,n] f c = eval cond f $ \res ->
  eval (if res == Bln False then n else y) f c

specialForm Lambda ((Lst ps):b) f c = c $ Fn ps False (Lst $ (SF Do):b) f
specialForm Macro  ((Lst ps):b) f c = c $ Fn ps True  (Lst $ (SF Do):b) f

specialForm Quote [v] _ c = c v

specialForm Quasiquote [val] f cont = spliceV val cont
  where
    spliceV (Lst [SF Splice, v]) = eval v f
    spliceV (Lst l) = spliceL l []
    spliceV v = ($v)
    spliceL [] l' c = c $ Lst l'
    spliceL ((Lst l):t) vs c
     | [SF Merge,  v] <- l = eval v f $ \v' -> case v' of
       Lst l' -> spliceL t (vs ++ l') c
       _ -> wispErr $ "ERROR: can't merge non-list: " ++ show v'
     | otherwise = spliceV (Lst l) $ \l' -> spliceL t (vs ++ [l']) c
    spliceL (v:t) vs c = spliceL t (vs ++ [v]) c

specialForm Splice _ _ _ = wispErr "ERROR: splice outside quasiquoted expression"
specialForm Merge _ _ _ = wispErr "ERROR: merge outside quasiquoted expression"

specialForm Def [s, xp] f c = eval xp f $ \v -> case destructure s v of
  Right kvs -> wispST (mapM_ (uncurry $ H.insert $ bindings f) kvs) >> c v
  Left err -> wispErr err

specialForm Set [Sym s, xp] f c = findBinding s f >>= \tf -> case tf of
  Just (_,tf') -> eval xp f $ \v -> wispST (H.insert (bindings tf') s v) >> c v
  _ -> wispErr $ "ERROR: set: free or immutable variable: " ++ unpack s

specialForm Undef [Sym s] f c = findBinding s f >>= \tf -> case tf of
  Just (_,tf') -> wispST (H.delete (bindings tf') s) >> c (Bln True)
  _ -> wispErr $ "ERROR: undef: free or immutable variable: " ++ unpack s

specialForm sf ps _ _ = wispErr $ "ERROR: syntax error in special form: " ++ show (Lst $ (SF sf):ps)

