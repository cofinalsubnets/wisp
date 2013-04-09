module Wisp.Types
( Wisp(..)
, Symbol
, Value(..)
, Frame(..)
, Form(..)
, wispErr
, wispIO
, pack
, unpack
, ArgSpec(..)
, Cont
, Continue
, anyNumber
) where

import qualified Data.HashTable.IO as H
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack, unpack)


type Cont = Value -> Wisp Value
type Continue = Cont -> Wisp Value

type Symbol = ByteString

newtype Wisp a = Wisp { unwrap :: IO (Either String a) }

instance Functor Wisp where
  fmap f (Wisp v) = Wisp $ fmap (fmap f) v

instance Monad Wisp where
  return = Wisp . return . return
  Wisp v >>= f = Wisp $ v >>= \r -> case r of
    Left err -> return $ Left err
    Right r' -> unwrap $ f r'

wispErr = Wisp . return . Left
wispIO = Wisp . fmap return

data ArgSpec = Exactly { count :: Int, guards :: [Value -> Bool]}
             | AtLeast { count :: Int, guards :: [Value -> Bool]}

anyNumber = AtLeast 0

data Frame = F { parent :: Maybe Frame
               , bindings :: H.BasicHashTable Symbol Value
               }

data Value = Int Int
           | Lst [Value]
           | Sym Symbol 
           | Bln Bool
           | Str String
           | Flt Double
           | Fn { params  :: [Value]
                , isMacro :: Bool
                , body    :: Value
                , closure :: Frame
                }
           | Prim { argSpec :: ArgSpec
                  , call    :: [Value] -> Frame -> Continue
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

instance Show Value where
  show (Int i) = show i
  show (Flt f) = show f
  show (Bln b) = if b then "#t" else "#f"
  show (Lst l) = "(" ++ unwords (map show l) ++ ")"
  show (Sym s) = unpack s
  show (Str s) = show s
  show (SF f) = show f
  show (Prim as _) = "#<fn/" ++ show (count as) ++ ">"
  show (Fn{params = ps}) = "#<fn/" ++ show (length ps) ++ ">"

instance Eq Value where
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


