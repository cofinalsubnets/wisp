{-# LANGUAGE RankNTypes, TupleSections #-}
module Wisp.Types
( Wisp
, Env(..)
, Symbol
, Value(..)
, Frame(..)
, Form(..)
, Output(..)
, wispErr
, wispST
, pack
, unpack
, ArgSpec(..)
, Continue
, anyNumber
, output
) where

import qualified Data.HashTable.ST.Cuckoo as HT
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack, unpack)
import Control.Monad.ST
import Control.Monad.Reader
import Control.Monad.Writer
import System.Random
import Data.Monoid


type Continue s = (Value s -> Wisp s (Value s)) -> Wisp s (Value s)

type Symbol = ByteString

type Wisp s a = ReaderT (Env s) (WriterT (Output Char) (ST s)) a

data Env s = Env { toplevel :: Frame s
                 , abort :: String -> Wisp s (Value s)
                 , input :: String
                 , randomSeed :: StdGen
                 }

newtype Output a = Output ([a] -> [a])

instance Monoid (Output a) where
  mempty = Output id
  mappend (Output a) (Output b) = Output $ a . b

output = Output . (++)

wispErr e = asks abort >>= ($ e ++ "\n")
wispST :: ST s a -> Wisp s a
wispST = ReaderT . const . WriterT . fmap (,mempty)

data ArgSpec = Exactly { count :: Int, guards :: forall s. [Value s -> Bool]}
             | AtLeast { count :: Int, guards :: forall s. [Value s -> Bool]}

anyNumber = AtLeast 0

data Frame s = F { parent :: Maybe (Frame s)
                 , bindings :: HT.HashTable s Symbol (Value s)
                 }

data Value s = Int Int
           | Lst [Value s]
           | Sym Symbol 
           | Bln Bool
           | Str String
           | Flt Double
           | Fn { params  :: [Value s]
                , isMacro :: Bool
                , body    :: Value s
                , closure :: Frame s
                }
           | Prim { argSpec :: ArgSpec
                  , call    :: [Value s] -> Continue s
                  }
           | SF Form

data Form = Do | If | Lambda | Def | Set | Macro | Quote | Quasiquote | Splice | Merge | Undef | Catch deriving Eq

instance Show Form where
  show Do         = "do"
  show If         = "if"
  show Lambda     = "fn"
  show Def        = "def"
  show Set        = "set"
  show Macro      = "macro"
  show Quote      = "quote"
  show Quasiquote = "quasiquote"
  show Splice     = "splice"
  show Merge      = "merge"
  show Undef      = "undef"
  show Catch      = "catch"

instance Show (Value s) where
  show (Int i) = show i
  show (Flt f) = show f
  show (Bln b) = if b then "#t" else "#f"
  show (Lst l) = "(" ++ unwords (map show l) ++ ")"
  show (Sym s) = unpack s
  show (Str s) = show s
  show (SF f) = show f
  show (Prim as _) = "#<fn/" ++ show (count as) ++ ">"
  show (Fn{params = ps}) = "#<fn/" ++ show (length ps) ++ ">"

instance Eq (Value s) where
  Int a == Int b = a == b
  Lst a == Lst b = a == b
  Sym a == Sym b = a == b
  Str a == Str b = a == b
  Bln a == Bln b = a == b
  Flt a == Flt b = a == b
  Int a == Flt b = fromIntegral a == b
  Flt a == Int b = a == fromIntegral b
  SF a  == SF b  = a == b
  _ == _ = False

instance Show ArgSpec where
  show (Exactly n _) = "exactly "  ++ show n
  show (AtLeast n _) = "at least " ++ show n

