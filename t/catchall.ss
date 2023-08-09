(module tests racket
  (provide runtests)
  (require "../s/test-driver.ss")

  (define (str s) (string-append "\"" s "\""))

  (define (runtests)
    (test-cases "strings"
      (test-case (string) (str ""))
      (test-case (string #\a) (str "a"))
      (test-case (string #\1 #\2 #\3) (str "123"))
    )

    (test-cases "complex constants"
      (test-case (quote 5) "5")
      (test-case (quote (5 . 1)) "(5 . 1)")
      (test-case (quote (1)) "(1)")
      (test-case "" (str ""))
      (test-case "a" (str "a"))
      (test-case (quote ("abc" . "def")) "(\"abc\" . \"def\")")
      (test-case (quote ("abc" "def")) "(\"abc\" \"def\")")
      (test-case (quote
                   ("abc" "de" #f
                    ("ghi" "jkl"
                     (#t #\u #\V
                      ("wxy" 0 ())))))
                 "(\"abc\" \"de\" #f (\"ghi\" \"jkl\" (#t #\\u #\\V (\"wxy\" 0 ()))))")

      #;(test-case (let ([f (lambda ()
                            (quote (1 . "H")))])
                   (eq? (f) (f)))
                 "#t")
      )


    (test-cases "macro expansion"
      (test-case (and) "#t")
      (test-case (and 1) "1")
      (test-case (and 1 2) "2")
      (test-case (and 1 2 3) "3")
      (test-case (and 1 2 3 #f) "#f")

      (test-case (or) "#f")
      (test-case (or 1) "1")
      (test-case (or #f 2) "2")
      (test-case (or #f 2 3) "2")
      (test-case (or #f #f 3) "3")
      (test-case (or #f #f #f 4) "4")

      (test-case
        (let ([sum (lambda (x y sum)
                     (cond
                       [(zero? x) y]
                       [(zero? y) x]
                       [(< y x) (sum (add1 x) (sub1 y) sum)]
                       [(< x y) (sum (add1 y) (sub1 x) sum)]
                       [else (* 2 x)]))])
          (cons (sum 10 0 sum)
                (cons (sum 0 10 sum)
                      (cons (sum 8 2 sum)
                            (cons (sum 2 8 sum)
                                  (cons (sum 5 5 sum) '()))))))
        "(10 10 10 10 10)")
        
      (test-case
        (list 1 2 3 4 5 6 7 8 9 10)
        "(1 2 3 4 5 6 7 8 9 10)")

      (test-case
        (let ([=q-helper
                (lambda (x alist =q-helper)
                  (cond
                    [(null? alist)
                     #f]
                    [(= (car (car alist)) x)
                     (car alist)]
                    [else (=q-helper x (cdr alist))]))])
          (let ([=q (lambda (x alist) (=q-helper x alist =q-helper))]
                [al (cons (cons 1 2) (cons (cons 3 4) '()))])
            (let ([val (lambda (x)
                         (cond
                           [(=q x al) => (lambda (as) (cdr as))]
                           [else #f]))])
              (cons (val 1)
                    (cons (val 3)
                          (cons (val 5) '()))))))
        "(2 4 #f)")

      (test-case
        (letrec ([len (lambda (l)
                        (cond
                          [(null? l) 0]
                          [else (add1 (len (cdr l)))]))])
          (len (list 1 2 3 4 5 6)))
        "6")

      ;;; Letrec bug
      ; TODO(mml): both of these should fail with a runtime exception about
      ; using a variable before it's defined
      #;(test-case (letrec ([a 10] [b (+ a a)]) (+ a b)) "30")
      #;(test-case
        (let* ([size 10000] [v (make-vector size 1)])
          (vector-length v))
        "10000")

      (test-case
        (let* ([a 10]
               [b (+ a 20)]
               [c (* b b)])
          c)
        "900")

      )

    (test-cases "assignment"
      (test-case
        ((((lambda (x)
             (let ([r #f])
               (lambda (y)
                 (lambda (z)
                   (set! r (+ x (+ y z)))
                   r))))
           10) 20) 30)
        "60")

      (test-case
        ((((lambda (x)
             (let ([r 0])
               (set! r (+ r x))
               (lambda (y)
                 (set! r (+ r y))
                 (lambda (z)
                   (set! r (+ r z))
                   r))))
           10) 20) 30)
        "60")

      (test-case
        (let ([make-acc (lambda ()
                          (let ([v 0])
                            (lambda (cmd arg)
                              (if (= cmd 0)
                                  v
                                  (if (= cmd 1)
                                      (set! v arg)
                                      (if (= cmd 2)
                                          (set! v (arg v))
                                          #f))))))]
              [acc-get (lambda (a) (a 0 '()))]
              [acc-set! (lambda (a n) (a 1 n))]
              [acc-apply! (lambda (a f) (a 2 f))])
          (let ([acc-add! (lambda (a n) (acc-apply! a (lambda (v) (+ v n))))]
                [acc-sub! (lambda (a n) (acc-apply! a (lambda (v) (- v n))))])
            (let ([a (make-acc)]
                  [b (make-acc)])
              (acc-add! a 10)
              (acc-add! b 100)
              (acc-sub! b (acc-get a))
              (acc-set! a 40)
              (- (acc-get b) (acc-get a)))))
        "50")


      (test-case
        (let ([a 10] [b 20])
          (set! a (begin
                    (set! b 1)
                    2))
          (+ a b))
        "3")
      )

    (test-cases "parsing challenges"
      (test-case ((lambda (lambda) (lambda lambda)) (lambda (let) 20))
                 "20")
      )

    (test-cases "begin"
      (test-case (begin 0) "0")
      (test-case (begin 0 10) "10"))

    (test-cases "Integer immediates"
      ; ARM cases
      ; Easy #1: 8 bit values
      (test-case 0 "0")
      (test-case 1 "1")
      (test-case 42 "42")
      (test-case 255 "255")

      ; Easy #2: powers of two
      (test-case 256 "256")
      (test-case 512 "512")
      (test-case 65536 "65536")
      ; 2^28
      (test-case 268435456 "268435456")

      ; Easy #3: n << m where n <= 255 and 0 <= m <= 15
      ; 42 << 15
      (test-case 1376256 "1376256")
      ; 255 << 14
      (test-case 4177920 "4177920")
      ; ; 255 << 15
      (test-case 8355840 "8355840")

      (test-case 257 "257")
      (test-case 4095 "4095")
      (test-case 65535 "65535")
      (test-case -1 "-1"))

    (test-cases "Non-integer immediates"
      ; booleans
      (test-case #t "#t")
      (test-case #f "#f")

      ; characters
      (test-case #\A "#\\A")

      ; null
      (test-case '() "()"))


    (test-cases "let"
      (test-case (let ([b 10]) b) "10")
      (test-case (let ([b 10])
                    (let ([b (+ b b)])
                      b))
                 "20")

      (test-case (let ([a 10]
                        [b 20])
                    (let ([b a]
                          [a b])
                      (- a b)))
                 "10")

      (test-case (let () 10 20) "20"))

    (test-cases "if"
      (test-case (if #t 20 30) "20")
      (test-case (if (< 0 1) 1 0) "1")
      (test-case (if (< 66 (char->integer #\A)) 9 5) "5")
      (test-case (if (< 66 (char->integer #\B)) 9 5) "5")
      (test-case (if (< 66 (char->integer #\C)) 9 5) "9")

      (test-case (let ([a (* (+ 30 70) (+ 35 65))])
                    (let ([b (if (< 9000 a)
                                 (* (* (* 10 9)
                                       (* 8 7))
                                    (* (* 6 5)
                                       (* 4 3)))
                                 (= (* (+ 10 20) (+ 30 40))
                                    (+ (* 10 (+ 30 40))
                                       (* 20 (+ 30 40)))))])
                      (* b 2)))
                 "3628800")

      (test-case (let ([a (* (+ 300 70) (+ 350 65))])
                    (let ([b (if (< 9000 a)
                                 (* (* (* 10 9)
                                       (* 8 7))
                                    (* (* 6 5)
                                       (* 4 3)))
                                 (= (* (+ 10 20) (+ 30 40))
                                    (+ (* 10 (+ 30 40))
                                       (* 20 (+ 30 40)))))])
                      (not (not (not (not b))))))
                 "#t"))

    (test-cases "cons"
      (test-case (car (cons 10 20)) "10")
      (test-case (cdr (cons 10 20)) "20")
      (test-case (car (cons 10 (cons 15 20))) "10")
      (test-case (cadr (cons 10 (cons 15 20))) "15")
      (test-case (cddr (cons 10 (cons 15 20))) "20")
      (test-case (let ([l (cons 1 (cons 2 (cons 3 ( cons 4 (cons 5 '())))))])
                    (car l))
                 "1")
      (test-case (let ([l (cons 1 (cons 2 (cons 3 ( cons 4 (cons 5 '())))))])
                    (car (cdr l))) "2")
      (test-case (let ([l (cons 1 (cons 2 (cons 3 ( cons 4 (cons 5 '())))))])
                    (caddr l)) "3")
      (test-case (let ([l (cons 1 (cons 2 (cons 3 ( cons 4 (cons 5 '())))))])
                    (cadr (cddr l))) "4")
      (test-case (let ([l (cons 1 (cons 2 (cons 3 ( cons 4 (cons 5 '())))))])
                    (caddr (cddr l))) "5")
      (test-case (let ([l (cons 1 (cons 2 (cons 3 ( cons 4 (cons 5 '())))))])
                    (null? (cdr (cddr (cddr l))))) "#t")
      (test-case (cons 10 20)
                 "(10 . 20)")
      (test-case (cons
                   (cons 10 (cons 20 '()))
                   (cons
                     (cons 30 (cons 40 '()))
                     '()))
                 "((10 20) (30 40))")
      (test-case (cons #\1 #\H) "(#\\1 . #\\H)")
      )

    (test-cases "mixed allocation"
      (test-case (cons (string #\a) 1) "(\"a\" . 1)")
      (test-case (cons 1 (string #\b)) "(1 . \"b\")")
      (test-case (cons (string #\a) (string #\b)) "(\"a\" . \"b\")")
      )


    (test-cases "tail calls"
      ; this one does no allocation, so it just pressures stack frames
      (test-case
        (let ([fxid-helper
                (lambda (n acc self)
                  (if (zero? n)
                      acc
                      (self (sub1 n) (add1 acc) self)))])
          (let ([fxid (lambda (n helper)
                        (helper n 0 helper))])
            (fxid 5000000 fxid-helper)))
        "5000000")

      (test-case
        (let ([add (lambda (a b) (+ a b))])
          (let ([f (lambda (add)
                     (let ([c 10] [d 20] [e 30] [f 40] [g 50] [h 60] [i 70] [j 80])
                       (* (add (add (add c d) e) f)
                          (add (add (add g h) i) j))))])
            (f add)))
        "26000")
      )

    (test-cases "closures"
      ; This closes over variables but it has no recursion and no tail calls.
      (test-case
        (let ([incr (lambda (x) (add1 x))])
          (let ([id (lambda (x) (sub1 (incr x)))])
            (id 10)))
        "10")

      ; Close over distant variables
      (test-case
        ((((lambda (x)
             (lambda (y)
               (lambda (z)
                 (+ z (+ x y)))))
           10) 20) 30)
        "60")

      ; Confuse me with names
      (test-case
        ((let ([ten 10])
           (lambda (x) (+ ten x)))
         20)
        "30")

      ; This one has recursion, but not in tail position.
      (test-case
        (let ([id-helper (lambda (in out self)
                           (if (zero? in)
                               out
                               (if (= 1 in)
                                   (add1 out)
                                   (add1 (self
                                           (- in 2)
                                           (+ out 1)
                                           self)))))])
          (let ([id (lambda (x)
                      (id-helper x 0 id-helper))])
            (id 10)))
        "10")

      ; This has only non-recursive tail calls
      (test-case
        (let ([incr (lambda (x c) (c (add1 x)))])
          (let ([id (lambda (x)
                      (incr x (lambda (x) (sub1 x))))])
            (id 10)))
        "10")
      
      ; Attempt to clobber closed-over variables.
      (test-case
        (let ([a 10] [b 100])
          (let ([*a (lambda (x c) (c (* a x)))]
                [*b (lambda (x c) (c (* b x)))])
            (let ([f (lambda ()
                       (*b 7 (lambda (hundreds)
                               (*a 2 (lambda (tens)
                                       (+ hundreds tens))))))])
              (f))))
        "720")

      )
    )

    #|
    These test cases lifted directly from tspl4.
    https://www.scheme.com/tspl4/objects.html#./objects:s10

    (test-cases "eq?"
      (test-case (eq? 'a 3)  "#f")
      (test-case (eq? #t 't)  "#f")
      (test-case (eq? "abc" 'abc)  "#f")
      (test-case (eq? "hi" '(hi))  "#f")
      (test-case (eq? #f '())  "#f")

      (test-case (eq? 9/2 7/2)  "#f")
      (test-case (eq? 3.4 53344)  "#f")
      (test-case (eq? 3 3.0)  "#f")
      (test-case (eq? 1/3 #i1/3)  "#f")

      (test-case (eq? 9/2 9/2)  "unspecified")
      (test-case (eq? 3.4 (+ 3.0 .4))  "unspecified")
      (test-case
        (let ([x (* 12345678987654321 2)])
         (eq? x x))
        "unspecified")

      (test-case (eq? #\a #\b)  "#f")
      (test-case (eq? #\a #\a)  "unspecified")
      (test-case (let ([x (string-ref "hi" 0)])
                   (eq? x x))  "unspecified")

      (test-case (eq? #t #t)  "#t")
      (test-case (eq? #f #f)  "#t")
      (test-case (eq? #t #f)  "#f")
      (test-case (eq? (null? '()) #t)  "#t")
      (test-case (eq? (null? '(a)) #f)  "#t")

      (test-case (eq? (cdr '(a)) '())  "#t")

      (test-case (eq? 'a 'a)  "#t")
      (test-case (eq? 'a 'b)  "#f")
      (test-case (eq? 'a (string->symbol "a"))  "#t")

      (test-case (eq? '(a) '(b))  "#f")
      (test-case (eq? '(a) '(a))  "unspecified")
      (test-case (let ([x '(a . b)]) (eq? x x))  "#t")
      (test-case (let ([x (cons 'a 'b)])
                   (eq? x x))  "#t")
      (test-case (eq? (cons 'a 'b) (cons 'a 'b))  "#f")

      (test-case (eq? "abc" "cba")  "#f")
      (test-case (eq? "abc" "abc")  "unspecified")
      (test-case (let ([x "hi"]) (eq? x x))  "#t")
      (test-case (let ([x (string #\h #\i)]) (eq? x x))  "#t")
      (test-case (eq? (string #\h #\i)
                      (string #\h #\i))  "#f")

      ;(test-case (eq? '#vu8(1) '#vu8(1))  "unspecified")
      ;(test-case (eq? '#vu8(1) '#vu8(2))  "#f")
      (test-case (let ([x (make-bytevector 10 0)])
                   (eq? x x))  "#t")
      (test-case (let ([x (make-bytevector 10 0)])
                   (eq? x (make-bytevector 10 0)))  "#f")

      (test-case (eq? '#(a) '#(b))  "#f")
      (test-case (eq? '#(a) '#(a))  "unspecified")
      (test-case (let ([x '#(a)]) (eq? x x))  "#t")
      (test-case (let ([x (vector 'a)])
                   (eq? x x))  "#t")
      (test-case (eq? (vector 'a) (vector 'a))  "#f")

      (test-case (eq? car car)  "#t")
      (test-case (eq? car cdr)  "#f")
      (test-case (let ([f (lambda (x) x)])
                   (eq? f f))  "#t")
      (test-case (let ([f (lambda () (lambda (x) x))])
                   (eq? (f) (f)))  "unspecified")
      (test-case (eq? (lambda (x) x) (lambda (y) y))  "unspecified")

      (test-case (let ([f (lambda (x)
                            (lambda ()
                              (set! x (+ x 1))
                              x))])
                   (eq? (f 0) (f 0)))  "#f")
      )
    |#
)
