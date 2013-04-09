module Wisp.Predicates
( (|||)
, anyValue
, number
, string
, symbol
, bool
, list
, primitive
, macro
, function
, applicable
, integer
, float
, strings
, numbers
, integers
, floats
, bools
, functions
, macros
, symbols
, lists
, primitives
, arguments
, admits
, satisfied
) where

import Wisp.Types

(|||) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
p1 ||| p2 = \s -> p1 s || p2 s

anyValue = const True

string (Str _) = True
string _ = False

integer (Int _) = True
integer _ = False

float (Flt _) = True
float _ = False

number = integer ||| float

bool (Bln _) = True
bool _ = False

function Fn{isMacro = m} = not m
function _ = False

macro Fn{isMacro = m} = m
macro _ = False

applicable = function ||| macro ||| primitive

symbol (Sym _) = True
symbol _ = False

list (Lst _) = True
list _ = False

primitive (Prim _ _) = True
primitive _ = False

strings    = repeat string
numbers    = repeat number
integers   = repeat integer
floats     = repeat float
bools      = repeat bool
functions  = repeat function
macros     = repeat macro
symbols    = repeat symbol
lists      = repeat list
primitives = repeat primitive
arguments  = []

satisfied :: Value -> Bool
satisfied Prim{argSpec = as} = count as == 0
satisfied Fn{params = ps} = length ps == 0

admits :: ArgSpec -> Value -> Bool
Exactly 0 _ `admits` _ = False
spec `admits` arg
 | null $ guards spec = True
 | otherwise = head (guards spec) arg

