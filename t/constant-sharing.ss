(module tests racket
  (provide runtests)
  (require "../s/test-driver.ss")

  (define (runtests)
    (test-cases "Constant sharing - lists"
      ; Two identical quoted lists should be the same object (shared)
      (test-case (eq? '(1 2) '(1 2)) "#t")
      (test-case (eq? '(a b c) '(a b c)) "#t")
      (test-case (eq? '() '()) "#t")
      (test-case (eq? '(1) '(1)) "#t")

      ; Nested lists should also be shared
      (test-case (eq? '((a b) (c d)) '((a b) (c d))) "#t")
      (test-case (eq? '(1 (2 3) 4) '(1 (2 3) 4)) "#t")

      ; Different lists should not be shared
      (test-case (eq? '(1 2) '(2 3)) "#f")
      (test-case (eq? '(a) '(b)) "#f"))

    (test-cases "Constant sharing - strings"
      ; Two identical quoted strings should be the same object
      (test-case (eq? "hello" "hello") "#t")
      (test-case (eq? "a" "a") "#t")
      (test-case (eq? "" "") "#t")

      ; Different strings should not be shared
      (test-case (eq? "hello" "world") "#f")
      (test-case (eq? "a" "b") "#f"))

    (test-cases "Constant sharing - symbols"
      ; Two identical quoted symbols should be the same object
      (test-case (eq? 'foo 'foo) "#t")
      (test-case (eq? 'x 'x) "#t")

      ; Different symbols should not be shared
      (test-case (eq? 'foo 'bar) "#f")
      (test-case (eq? 'a 'b) "#f"))

    (test-cases "Constant sharing - vectors"
      ; Two identical quoted vectors should be the same object
      (test-case (eq? '#(1 2 3) '#(1 2 3)) "#t")
      (test-case (eq? '#(a b) '#(a b)) "#t")
      (test-case (eq? '#() '#()) "#t")

      ; Different vectors should not be shared
      (test-case (eq? '#(1 2) '#(2 3)) "#f")
      (test-case (eq? '#(a) '#(b)) "#f"))

    (test-cases "Constant sharing - complex nested"
      ; Complex nested structures
      (test-case (eq? '(#(a b) "str" foo) '(#(a b) "str" foo)) "#t")
      (test-case (eq? '("x" ("y" "z")) '("x" ("y" "z"))) "#t")

      ; Verify different nested structures don't share
      (test-case (eq? '(#(1) "a") '(#(2) "a")) "#f"))

    (test-cases "Constant sharing in functions"
      ; Constants should be shared even inside function bodies
      (test-case
        (let ([f (lambda () '(1 2 3))]
              [g (lambda () '(1 2 3))])
          (eq? (f) (g)))
        "#t")

      ; Multiple calls to same function return same constant
      (test-case
        (let ([f (lambda () '(x y z))])
          (eq? (f) (f)))
        "#t"))))
