(test "math functions"
  ("equality"
   (and (= 1 1.0)
        (= 1 1)
        (= 1.0 1)
        (= 1.0 1.0)
        (not (= 1 2))
        (not (= 1.6 -312542))))

  ("integer division"
    (= 1 (/ 3 2)))

  ("floating point division"
    (= 1.5
       (/ 3 2.0)
       (/ 3.0 2)
       (/ 3.0 2.0))))

(test "library functions"

  ("map"
    (= (map inc '(1 2 3)) '(2 3 4)))

  ("fold"
    (= (fold (flip cons) '() '(1 2 3 4 5)) '(5 4 3 2 1)))

  ("filter"
    (= (filter number? '(1 2 'a "wat" (pfffff))) '(1 2)))

  ("comp"
    (= '(#t) ((comp list symbol?) 'sym)))

  ("type predicates"
   (and (list? '(1))
        (number? 72165972.451)
        (string? "no way bro")
        (symbol? 'hahawow)
        (bool? #f)
        (not (string? 'wat))
        (not (list? "glug"))
        (not (primitive? list))
        (primitive? +)
        (not (number? "pew pew pew")))))

(test "currying"
  ("currying"
    (let ((ap (fn (op a b) (op a b))))
      (and (= (+ 1 2) (((ap +) 1) 2))
           (= "abc" ((ap str) "a" "bc"))
           (= '(a (b c)) (((ap list) 'a) '(b c))))))

  ("let, currying, and scoping"
    (let ((a 1) (b 2) (c 3) (add3 (fn (c d e) (+ c d e))))
      (and (= a 1)
           (= b 2)
           (= c 3)
           (= 6 (add3 a b c))
           (= 6 (((add3 a) b) c))))))

(test "list destructuring"
  ("destructuring"
   (let (((a (b c) & d) '(1 (2 3) 4 5 6)))
     (and (= 1 a)
          (= 2 b)
          (= 3 c)
          (= '(4 5 6) d))))

  ("destructuring with currying"
   (let ((f (fn (a (b & c)) (list a b c))))
     (= '(1 2 (3)) ((f 1) '(2 3)))))

  ("destructuring in definitions"
   (do
     (def (a b c (d e) & f) '(1 2 3 (4 5) 6 7 8))
     (and (= a 1)
          (= b 2)
          (= c 3)
          (= d 4)
          (= e 5)
          (= f '(6 7 8))))))

(test "continuations"
  ("escaping"
    (do (def ok #t)
      (and (= 7 (call/cc (fn (cc) 1 2 (cc 7) (set ok #f))))
           ok))))

