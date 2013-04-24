module Wisp.STL (stl) where

import Wisp.Reader

stl = case parseWisp l of
  Right v -> v
  Left err -> error $ show err
  where
    l = unlines $
     [ "(do"
     , "  (def defm (macro (name args & body)"
     , "    `(def ,name (macro ,args @body))))"
     , "  (defm defn (name args & body)"
     , "    `(def ,name (fn ,args @body)))"
     , "  (defn list (& as) as)"
     , "  (defn loop (f) (f) (loop f))"
     , "  (defn id (n) n)"
     , "  (defn println (s) (print s \"\n\"))"
     , "  (defn repl ()"
     , "    (loop (fn () (println (catch id (eval (read (get-line))))))))"
     , ")"
     ]

