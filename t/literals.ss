(module tests racket
  (provide runtests)
  (require "../s/test-driver.ss")

  (define (runtests)
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

    (test-cases "Direct literal printing - strings"
      (test-case "a" "\"a\"")
      (test-case "x" "\"x\"")
      (test-case "z" "\"z\"")
      (test-case "hello" "\"hello\""))

    (test-cases "Direct literal printing - symbols"
      (test-case 'a "a")
      (test-case 'x "x")
      (test-case 'z "z")
      (test-case 'hello "hello"))

    (test-cases "Direct literal printing - numbers"
      (test-case '(1) "(1)")
      (test-case '(1 2) "(1 2)")
      (test-case '(1 2 3) "(1 2 3)"))

    (test-cases "Direct literal printing - lists with symbols"
      (test-case '(a) "(a)")
      (test-case '(x) "(x)")
      (test-case '(a b) "(a b)")
      (test-case '(a b c) "(a b c)"))

    (test-cases "Direct literal printing - mixed"
      (test-case '(1 a 2) "(1 a 2)")
      (test-case '(a 1) "(a 1)")
      (test-case '(a 1 b 2) "(a 1 b 2)"))

    (test-cases "Direct literal printing - nested"
      (test-case '((a)) "((a))")
      (test-case '((a b)) "((a b))")
      (test-case '(a (b)) "(a (b))")
      (test-case '((a) b) "((a) b)"))))
