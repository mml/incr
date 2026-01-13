(module tests racket
  (provide runtests)
  (require "../s/test-driver.ss")
  (define (runtests)
    (test-cases "Binary primitives"
      ; and - truth table
      (test-case (and #f #f) "#f")
      (test-case (and #f #t) "#f")
      (test-case (and #t #f) "#f")
      (test-case (and #t #t) "#t")

      ; or - truth table
      (test-case (or #f #f) "#f")
      (test-case (or #f #t) "#t")
      (test-case (or #t #f) "#t")
      (test-case (or #t #t) "#t")

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

      ; >
      (test-case (> 0 1) "#f")
      (test-case (> 1 0) "#t")
      (test-case (> 0 0) "#f")

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
                 "#t")
      
      ;; bit shifting
      (test-case (bitwise-arithmetic-shift 1 10) "1024")
      (test-case (bitwise-arithmetic-shift-left 1 10) "1024")
      (test-case (bitwise-arithmetic-shift-right 1 -10) "1024")

      (test-case (bitwise-arithmetic-shift 65536 -6) "1024")
      (test-case (bitwise-arithmetic-shift-left 65536 -6) "1024")
      (test-case (bitwise-arithmetic-shift-right 65536 6) "1024")

      (test-case (bitwise-arithmetic-shift-right 1 1) "0")

      (test-case (bitwise-arithmetic-shift -1 10) "-1024")
      (test-case (bitwise-arithmetic-shift-left -1 10) "-1024")
      (test-case (bitwise-arithmetic-shift-right -1 -10) "-1024")

      (test-case (bitwise-arithmetic-shift -65536 -6) "-1024")
      (test-case (bitwise-arithmetic-shift-left -65536 -6) "-1024")
      (test-case (bitwise-arithmetic-shift-right -65536 6) "-1024")

      (test-case (bitwise-arithmetic-shift-right -1 1) "-1")
      (test-case (bitwise-arithmetic-shift -65536 -15) "-2")
      (test-case (bitwise-arithmetic-shift -65536 -16) "-1")
      (test-case (bitwise-arithmetic-shift -65536 -17) "-1")
      (test-case (bitwise-arithmetic-shift -65536 -30) "-1")
      (test-case (bitwise-arithmetic-shift -65536 -32) "-1")

      ;; quotient
      (test-case (quotient 10 3) "3")
      (test-case (quotient 10 5) "2")
      (test-case (quotient 7 7) "1")
      (test-case (quotient 5 10) "0")
      (test-case (quotient -10 3) "-3")
      (test-case (quotient -10 5) "-2")
      (test-case (quotient -7 7) "-1")
      (test-case (quotient 10 -3) "-3")
      (test-case (quotient 10 -5) "-2")
      (test-case (quotient -10 -3) "3")
      (test-case (quotient -10 -5) "2")
      (test-case (quotient 42 1) "42")
      (test-case (quotient -42 1) "-42")
      (test-case (quotient (quotient 100 5) 2) "10")

      ; From tspl4: https://www.scheme.com/tspl4/objects.html
      (test-case (quotient 45 6) "7")

      ;; remainder
      (test-case (remainder 10 3) "1")
      (test-case (remainder 10 5) "0")
      (test-case (remainder 7 7) "0")
      (test-case (remainder 5 10) "5")
      (test-case (remainder -10 3) "-1")
      (test-case (remainder -10 5) "0")
      (test-case (remainder -7 3) "-1")
      (test-case (remainder 10 -3) "1")
      (test-case (remainder 10 -5) "0")
      (test-case (remainder -10 -3) "-1")
      (test-case (remainder -10 -5) "0")
      (test-case (= 10 (+ (* (quotient 10 3) 3) (remainder 10 3))) "#t")
      (test-case (= -10 (+ (* (quotient -10 3) 3) (remainder -10 3))) "#t")
      (test-case (= 10 (+ (* (quotient 10 -3) -3) (remainder 10 -3))) "#t")
      (test-case (= -10 (+ (* (quotient -10 -3) -3) (remainder -10 -3))) "#t")

      ; From tspl4: https://www.scheme.com/tspl4/objects.html
      (test-case (remainder 16 4) "0")
      (test-case (remainder 5 2) "1")
      (test-case (remainder -17 -9) "-8")

      ;; modulo
      (test-case (modulo 10 3) "1")
      (test-case (modulo 10 5) "0")
      (test-case (modulo 7 7) "0")
      (test-case (modulo -10 3) "2")
      (test-case (modulo -7 3) "2")
      (test-case (modulo -10 5) "0")
      (test-case (modulo 10 -3) "-2")
      (test-case (modulo 7 -3) "-2")
      (test-case (modulo -10 -3) "-1")
      (test-case (modulo -10 -5) "0")
      (test-case (= (remainder -10 3) (modulo -10 3)) "#f")
      (test-case (= (remainder 10 -3) (modulo 10 -3)) "#f")
      (test-case (= (remainder 10 3) (modulo 10 3)) "#t")

      ; From tspl4: https://www.scheme.com/tspl4/objects.html
      (test-case (modulo 16 4) "0")
      (test-case (modulo 5 2) "1")
      (test-case (modulo -17 -9) "-8")
      )
    ))
