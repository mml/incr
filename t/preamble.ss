(module tests racket
  (provide runtests)
  (require "../s/test-driver.ss")

  (define (runtests)
    (test-cases "Preamble: append"
      (test-case
        (append '(1 2) '(3 4))
        "(1 2 3 4)")

      (test-case
        (append '() '(5 6))
        "(5 6)")

      (test-case
        (append '(10) '())
        "(10)")

      (test-case
        (append '() '())
        "()"))))
