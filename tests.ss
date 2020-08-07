(require "test-driver.ss")

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
  (test-case () "()"))

(test-cases "Unary primitives"
  ;;; unary primitives
  ; add1
  (test-case (add1 0) "1")
  (test-case (add1 (add1 0)) "2")
  (test-case (add1 (add1 -2)) "0")

  ; sub1
  (test-case (sub1 0) "-1")
  (test-case (sub1 (sub1 0)) "-2")
  (test-case (sub1 (sub1 2)) "0")

  ; zero?
  (test-case (zero? 0) "#t")
  (test-case (zero? 1) "#f")
  (test-case (zero? -1) "#f")
  (test-case (zero? #\t) "#f")
  (test-case (zero? #\f) "#f")

  ; not
  (test-case (not #f) "#t")
  (test-case (not #t) "#f")
  (test-case (not 0) "#f")

  ; null?
  (test-case (null? ()) "#t")
  (test-case (null? #f) "#f")

  (test-case (sub1 (add1 0)) "0")
  (test-case (add1 (sub1 0)) "0")

  (test-case (sub1 (add1 123456789)) "123456789")
  (test-case (add1 (sub1 123456789)) "123456789")

  ;; integer<->char
  (test-case (integer->char 65) "#\\A")
  (test-case (char->integer #\A) "65")
  (test-case (integer->char (add1 (char->integer #\l))) "#\\m"))

(test-cases "Binary primitives"
  ; +
  (test-case (+ 2 2) "4")
  (test-case (+ 0 0) "0")
  (test-case (+ -1000 1000) "0")
  (test-case (+ 2048 2048) "4096")
  (test-case (+ (+ (+ 1 2)
                    (+ 3 4))
                 (+ (+ 5 6)
                    (+ 7 8))) "36")

  ; -
  (test-case (- 4 2) "2")
  (test-case (- 0 0) "0")
  (test-case (- 0 1000) "-1000")
  (test-case (- 4096 2048) "2048")
  (test-case (- (- (- 2048 1024)
                    (- 1024 512))
                 (- (- 512 256)
                    (- 256 128))) "384")


  (test-case (+ (- 4 2) (- 8 6)) "4")
  (test-case (- (+ 100 100) (+ 10 10)) "180")

  ; =
  (test-case (= 1 1) "#t")
  (test-case (= 1 2) "#f")
  (test-case (not (= 1 2)) "#t")

  (test-case (= (+ 5 5) (+ 9 1)) "#t")
  (test-case (= (- 30 10) (- 105 85)) "#t")

  ; <
  (test-case (< 0 1) "#t")
  (test-case (< 1 0) "#f")
  (test-case (< 0 0) "#f")

  ; *
  (test-case (* 1 0) "0")
  (test-case (* 0 1) "0")
  (test-case (* 1 1) "1")
  (test-case (* 10 47) "470")
  (test-case (* 47 10) "470")
  (test-case (* -10 47) "-470")
  (test-case (* -47 10) "-470")
  (test-case (* (* (* 10 9)
                    (* 8 7))
                 (* (* 6 5)
                    (* 4 3))) "1814400")

  (test-case (* (+ 30 70) (+ 35 65)) "10000")
  (test-case (* (- 70 30) (- 90 50)) "1600")
  (test-case (= (* (+ 10 20) (+ 30 40))
                 (+ (* 10 (+ 30 40))
                    (* 20 (+ 30 40))))
             "#t"))

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
  (test-case (let ([l (cons 1 (cons 2 (cons 3 ( cons 4 (cons 5 ())))))])
                (car l))
             "1")
  (test-case (let ([l (cons 1 (cons 2 (cons 3 ( cons 4 (cons 5 ())))))])
                (car (cdr l))) "2")
  (test-case (let ([l (cons 1 (cons 2 (cons 3 ( cons 4 (cons 5 ())))))])
                (caddr l)) "3")
  (test-case (let ([l (cons 1 (cons 2 (cons 3 ( cons 4 (cons 5 ())))))])
                (cadr (cddr l))) "4")
  (test-case (let ([l (cons 1 (cons 2 (cons 3 ( cons 4 (cons 5 ())))))])
                (caddr (cddr l))) "5")
  (test-case (let ([l (cons 1 (cons 2 (cons 3 ( cons 4 (cons 5 ())))))])
                (null? (cdr (cddr (cddr l))))) "#t")
  (test-case (cons 10 20)
             "(10 . 20)")
  (test-case (cons
               (cons 10 (cons 20 ()))
               (cons
                 (cons 30 (cons 40 ()))
                 ()))
             "((10 20) (30 40))"))

(test-cases "procedures"
  (test-case
    (let ([ten (lambda () 10)])
      (ten))
    "10")

  (test-case
    (let ([eleven (lambda () (add1 10))])
      (eleven))
    "11")

  (test-case
    (let ([double (lambda (x) (* x 2))])
      (double 10))
    "20")

  (test-case
    (let ([double (lambda (x) (* x 2))])
      (double (double 10)))
    "40")

  (test-case
    (let ([double (lambda (x) (* x 2))]
          [triple (lambda (x) (* x 3))])
      (= (double (triple #xff0000))
         (triple (double #xff0000))))
    "#t")

  (test-case
    (let ([add (lambda (a b) (+ a b))])
      (add 20 20))
    "40")

  (test-case
    (let ([g (lambda (f) (f 20 20))]
          [add (lambda (a b) (+ a b))])
      (g add))
    "40")

  (test-case (let ([add (lambda (x y) (+ x y))]
                   [mul (lambda (x y) (* x y))])
               (mul
                 (add 10 15)
                 (add 20 25)))
             "1125")
  
  (test-case (let ([fxid (lambda (n self) (if (zero? n) n (add1 (self (sub1 n) self))))])
               (fxid 0 fxid))
             "0")

  (test-case (let ([fxid (lambda (n self) (if (zero? n) n (add1 (self (sub1 n) self))))])
               (fxid 1 fxid))
             "1")

  (test-case (let ([fxid (lambda (n self) (if (zero? n) n (add1 (self (sub1 n) self))))])
               (fxid 2 fxid))
             "2")

  (test-case (let ([len (lambda (l len) (if (null? l) 0 (+ 1 (len (cdr l) len))))])
               (len () len))
             "0")

  (test-case (let ([mkl (lambda (n self) (if (zero? n) () (cons #f (self (sub1 n) self))))])
               (mkl 5 mkl))
             "(#f #f #f #f #f)")
  )
(test-cases skip "old procedures"
  (test-case (labels ([len (code (l) (if (null? l) 0 (+ 1 (labelcall len (cdr l)))))])
      (labelcall len ()))
    "0")
  (test-case (labels ([len (code (l) (if (null? l) 0 (+ 1 (labelcall len (cdr l)))))])
      (labelcall len (cons 1 ())))
    "1")
  (test-case (labels ([len (code (l) (if (null? l) 0 (+ 1 (labelcall len (cdr l)))))])
      (labelcall len (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 ())))))))
    "5")
  (test-case (labels ([fib (code (n) (if (zero? n) 1
                                         (if (= 1 n) 1
                                             (+ (labelcall fib (- n 1))
                                                (labelcall fib (- n 2))))))])
                     (labelcall fib 33))
             "5702887")
  
  )
