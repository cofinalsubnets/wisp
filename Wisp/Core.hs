{-# LANGUAGE PatternGuards #-}
module Wisp.Core
( lookup
, eval
, apply
) where

import Wisp.Types
import Wisp.Predicates
import Prelude hiding (lookup)
import qualified Data.HashTable.IO as H
import Control.Monad (foldM)

-- | name resolution
lookup :: Symbol -> Frame -> Wisp Value
lookup k = lu . Just
  where
    lu Nothing = wispErr $ "Unable to resolve symbol: " ++ unpack k
    lu (Just f) = wispIO (H.lookup (bindings f) k) >>= maybe (lu $ parent f) return

eval :: Value -> Frame -> Continue
eval v f cont = case v of
 Sym s -> cont =<< lookup s f
 Lst ((SF m):ps) -> specialForm m ps f cont
 Lst (o:ps) -> eval o f $ \o' ->
   if macro o' then apply o' ps f $ \r -> eval r f cont
   else let c' [] ps'     = apply o' (reverse ps') f cont
            c' (a:as) ps' = eval a f $ \a' -> c' as (a':ps')
        in c' ps []
 _ -> cont v

apply :: Value -> [Value] -> Frame -> Continue
apply proc args frame cont
 | primitive proc = foldM apC proc args >>= invoke frame cont
 | applicable proc = case destructure (Lst bound) (Lst args) of
   Left err -> wispErr err
   Right kvs -> do
     f <- mkFrame (Just $ closure proc) kvs
     invoke frame cont proc{params = drop (length bound) $ params proc, closure = f}
 | otherwise = wispErr $ "Non-applicable value: " ++ show proc

 where

    (pos, var) = break (== splat) $ params proc
    (bound, unbound) = if length args >= length pos then (params proc,[])
                       else splitAt (length args) (params proc)

    invoke f c pr
     | satisfied pr = case pr of
       Prim{} -> call pr [] f c
       Fn{closure = cl, body = b} -> eval b cl c
     | otherwise = c pr

    Prim {argSpec = as, call = p} `apC` arg
     | as `admits` arg = return Prim {argSpec = nextSpec as, call = p . (arg:)}
     | otherwise = wispErr $ "Bad type: " ++ show arg

    nextSpec s = s{count = max 0 (pred $ count s), guards = drop 1 $ guards s}

    mkFrame p bs = Wisp $ H.fromList bs >>= \ht -> return (return $ F p ht)

-- SPECIAL FORMS

specialForm :: Form -> [Value] -> Frame -> Continue

specialForm Do [p] f c = eval p f c
specialForm Do (p:ps) f c = eval p f $ \_ -> specialForm Do ps f c

specialForm If [cond,y,n] f c = eval cond f $ \res ->
  eval (if res == Bln False then n else y) f c

specialForm Lambda ((Lst ps):b) f c = c $ Fn ps False (Lst $ (SF Do):b) f
specialForm Macro  ((Lst ps):b) f c = c $ Fn ps True  (Lst $ (SF Do):b) f

specialForm Quote [v] _ c = c v

specialForm Quasiquote [val] f cont = spliceV val cont
  where
    spliceV (Lst [SF Splice, v]) c = eval v f c
    spliceV (Lst l) c = spliceL l [] c
    spliceV v c = c v
    spliceL [] l' c = c $ Lst l'
    spliceL ((Lst l):t) vs c
     | [SF Merge,  v] <- l = eval v f $ \v' -> case v' of
       Lst l' -> spliceL t (vs ++ l') c
       _ -> wispErr $ "ERROR: can't merge non-list: " ++ show v'
     | otherwise = spliceV (Lst l) $ \l' -> spliceL t (vs ++ [l']) c
    spliceL (v:t) vs c = spliceL t (vs ++ [v]) c

specialForm Splice _ _ _ = wispErr "ERROR: splice outside quasiquoted expression"
specialForm Merge _ _ _ = wispErr "ERROR: merge outside quasiquoted expression"

specialForm Def [s, xp] f c = eval xp f $ \v ->
  case destructure s v of
    Left err -> wispErr err
    Right kvs -> wispIO (mapM_ (uncurry $ H.insert $ bindings f) kvs) >> c v

specialForm Set [Sym s, xp] f c = findBinding s f >>= \tf -> case tf of
  Just tf' -> eval xp f $ \v -> do
    wispIO $ H.insert (bindings tf') s v
    c v
  _ -> wispErr $ "ERROR: set: free or immutable variable: " ++ unpack s

specialForm Undef [Sym s] f c = findBinding s f >>= \tf -> case tf of
  Just tf' -> do
    wispIO $ H.delete (bindings tf') s
    c $ Bln True
  _ -> wispErr $ "ERROR: undef: free or immutable variable: " ++ unpack s

specialForm Catch (h:xp) f c = do
  res <- wispIO $ do
    r <- unwrap $ eval (Lst $ (SF Do):xp) f return
    case r of Right r' -> return $ c r'
              Left err -> return $ eval h f $ \h' -> apply h' [Str err] f c
  res

specialForm sf ps _ _ = wispErr $ "ERROR: syntax error in special form: " ++ show (Lst $ (SF sf):ps)


findBinding :: Symbol -> Frame -> Wisp (Maybe Frame)
findBinding nm f = maybe iter (\_ -> return $ return f) =<< wispIO (H.lookup (bindings f) nm)
  where iter = maybe (return Nothing) (findBinding nm) (parent f)

-- LIST DESTRUCTURING

destructure :: Value -> Value -> Either String [(Symbol, Value)]

destructure (Sym s) v = Right [(s,v)]

destructure (Lst l) (Lst v) 
  | (req, (_:o)) <- break (== splat) l = do
    let ps = length req
    pos <- destructure (Lst req) (Lst $ take ps v)
    case o of [s] -> fmap (pos ++) $ destructure s (Lst $ drop ps v)
              s -> Left $ "Pattern error: bad splat: " ++ show (Lst l)
  | length l == length v = fmap concat . sequence $ zipWith destructure l v
  | otherwise = structError (Lst l) (Lst v)

destructure l@(Lst _) v = structError l v

destructure p _ = Left $ "Pattern error: illegal pattern: " ++ show p

structError l v = Left . unwords $
  [ "Pattern error: structure mismatch:" , show l , "<-" , show v ]

splat = Sym $ pack "&"

