wisp
====
wisp is a tiny interpreted lisp written in haskell & easily embedded into larger haskell programs, e.g., as a scripting language. It includes a superset of the following features:

- full lexical closures
- tail-call optimization
- macros
- first-class continuations
- pattern matching (on lists)
- automatic currying

the wisp interpreter
====================

wisp lives in the ST (optionally IO) monad. Separate interpreters with completely segregated environments can be run concurrently. wisp has a few IO facilities, but they're completely sandboxed and can't accidentally affect the host program or environment.

