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
      , "  (defm cond (& cases)"
      , "    (fold (fn (l c)"
      , "            `(if ,(car c)"
      , "                 (do @(cdr c))"
      , "                 ,l))"
      , "          '(error \"cond: fell through\")"
      , "          (reverse cases)))"

      , "  (defm let (binds & body)"
      , "    `((fn ,(map car binds) @body) @(map cadr binds)))"

      , "  (defn member (e lst)"
      , "    (cond ((null? lst) #f)"
      , "          ((= (car lst) e) lst)"
      , "          (#t (member e (cdr lst)))))"

      , "  (defn length (l)"
      , "    (if (null? l)"
      , "        0"
      , "        (+ 1 (length (cdr l)))))"

      , "  (defn null? (l)"
      , "    (if (list? l)"
      , "      (= l '())"
      , "      (error (str \"ERROR: Bad type: \" l))))"

      , "  (defn flip (f a b) (f b a))"

      , "  (defn comp (f g)"
      , "    (fn (n) (f (g n))))"

      , "  (defn car (l)"
      , "    (if (null? l)"
      , "        (error \"car: null list\")"
      , "        (apply (fn (h & _) h)"
      , "               l)))"

      , "  (defn cdr (l)"
      , "    (if (null? l)"
      , "        l"
      , "        (apply (fn (_ & t) t)"
      , "               l)))"

      , "  (defn /= (a b) (not (= a b)))"
      , "  (defn > (a b) (and (not (= a b))"
      , "                       (not (< a b))))"
      , "  (defn >= (a b) (or (= a b) (> a b)))"
      , "  (defn <= (a b) (or (= a b) (< a b)))"

      , "  (def caar (comp car car))"
      , "  (def cadr (comp car cdr))"
      , "  (def cddr (comp cdr cdr))"
      , "  (def cdar (comp cdr car))"

      , "  (def caaar (comp car caar))"
      , "  (def caadr (comp car cadr))"
      , "  (def cadar (comp car cdar))"
      , "  (def caddr (comp car cddr))"
      , "  (def cdaar (comp cdr caar))"
      , "  (def cdadr (comp cdr cadr))"
      , "  (def cddar (comp cdr cdar))"
      , "  (def cdddr (comp cdr cddr))"

      , "  (defn assoc (v l)"
      , "    (cond ((null? l) #f)"
      , "          ((= v (caar l)) (cadar l))"
      , "          (#t (assoc v (cdr l)))))"

      , "  (defn map (op l)"
      , "    (if (null? l)"
      , "        l"
      , "        (cons (op (car l))"
      , "              (map op (cdr l)))))"

      , "  (defn filter (p l)"
      , "    (cond ((null? l) l)"
      , "          ((p (car l))"
      , "           (cons (car l)"
      , "                 (filter p (cdr l))))"
      , "          (#t (filter p (cdr l)))))"

      , "  (defn fold (op acc l)"
      , "    (if (null? l)"
      , "        acc"
      , "        (fold op (op acc (car l)) (cdr l))))"

      , "  (defn cons (new-car new-cdr)"
      , "    (if (list? new-cdr)"
      , "     `(,new-car @new-cdr)"
      , "     (error (str \"Bad type: \" new-cdr))))"

      , "  (defn not (v) (if v #f #t))"
      , "  (defn && (a b) (if a b a))"
      , "  (defn || (a b) (if a a b))"
      , "  (defm and (& clauses)"
      , "    (let (((cls1 & clss) (reverse clauses)))"
      , "      (fold (fn (prev cur) `(let ((current-clause ,cur)) (if current-clause ,prev current-clause))) cls1 clss)))"
      , "  (defm or (& clauses)"
      , "    (let (((cls1 & clss) (reverse clauses)))"
      , "      (fold (fn (prev cur) `(let ((current-clause ,cur)) (if current-clause current-clause ,prev))) cls1 clss)))"

      , "  (defn reverse (l)"
      , "    (defn inner (acc l)"
      , "      (if (null? l)"
      , "          acc"
      , "          (inner (cons (car l) acc)"
      , "                 (cdr l))))"
      , "    (inner '() l))"

      , "  (defn inc (n) (+ n 1))"
      , "  (defn dec (n) (- n 1))"
      , "  (defn id (n) n)"
      , "  (defn const (x) (fn (y) x))"

      , "  (defn rem (a b)"
      , "    (- a (* b (/ a b))))"

      , "  (def floor int)"

      , "  (defn ceil (n)"
      , "    (let ((f (floor n)))"
      , "      (if (= n f) f (inc f))))"

      , "  (defn abs (n)"
      , "    (if (< n 0)"
      , "      (* n -1)"
      , "      n))"

      , "  (defn append (l1 l2)"
      , "    (if (null? l1)"
      , "        l2"
      , "        (cons (car l1)"
      , "              (append (cdr l1) l2))))"

      , "  (defn juxt (f g)"
      , "    (fn (& t)"
      , "      (list (apply f t)"
      , "            (apply g t))))"

      , "  (defn join (strs j)"
      , "    (fold (fn (s1 s2) (str s1 j s2)) (car strs) (cdr strs)))"

      , "  (defn println (s) (print s \"\n\"))"

      , "  (defm test (suite & ts)"
      , "    (if (list? suite)"
      , "        (set ts (cons suite ts))"
      , "        (print \"Testing \" suite \" \"))"
      , "    (defn pass () (print \".\"))"
      , "    (defn fail () (print \"X\"))"
      , "    (def failures '())"
      , "    (map (fn (x)"
      , "           (let (((doc t) x))"
      , "             (if (eval t)"
      , "                 (pass)"
      , "                 (do (set failures (cons doc failures))"
      , "                     (fail)))))"
      , "         ts)"
      , "    (if (null? failures)"
      , "      (do (println \" ok!\") #t)"
      , "      (do"
      , "        (println (str \"\n\" (length failures) \" failure(s): \"))"
      , "        (map println failures)"
      , "        #f)))"
      , ")"
      , ")"
      ]

