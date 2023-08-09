(module tests racket
  (provide runtests)
  (require "../s/test-driver.ss")
  (define (runtests)
    (test-cases "Nullary primitives"
      (test-case (void) "#<void>"))

    (test-cases "Unary primitives"
      ;;; unary primitives
      ; add1
      (test-case (add1 0) "1")
      (test-case (add1 (add1 0)) "2")
      (test-case (add1 (add1 -2)) "0")

      ; sub1
      (test-case (sub1 2) "1")
      (test-case (sub1 1) "0")
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
      (test-case (null? '()) "#t")
      (test-case (null? #f) "#f")

      (test-case (sub1 (add1 0)) "0")
      (test-case (add1 (sub1 0)) "0")

      (test-case (sub1 (add1 123456789)) "123456789")
      (test-case (add1 (sub1 123456789)) "123456789")

      ;; integer<->char
      (test-case (integer->char 65) "#\\A")
      (test-case (char->integer #\A) "65")
      (test-case (integer->char (add1 (char->integer #\l))) "#\\m")
      )
    ))
