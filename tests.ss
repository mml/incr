(load "test-driver.ss")
(load "compiler.ss")

; integer immediates
; ARM cases
; Easy #1: 8 bit values
(test-case '0 "0")
(test-case '1 "1")
(test-case '42 "42")
(test-case '255 "255")

; Easy #2: powers of two
(test-case '256 "256")
(test-case '512 "512")
(test-case '65536 "65536")
; 2^28
(test-case '268435456 "268435456")

; Easy #3: n << m where n <= 255 and 0 <= m <= 15
; 42 << 15
(test-case '1376256 "1376256")
; 255 << 14
(test-case '4177920 "4177920")
; ; 255 << 15
(test-case '8355840 "8355840")

(test-case '257 "257")
(test-case '4095 "4095")
(test-case '65535 "65535")
(test-case '-1 "-1")

; booleans
(test-case '#t "#t")
(test-case '#f "#f")

; characters
(test-case '#\A "#\\A")

; null
(test-case '() "()")

;;; unary primitives
; add1
(test-case '(add1 0) "1")
(test-case '(add1 (add1 0)) "2")
(test-case '(add1 (add1 -2)) "0")

; sub1
(test-case '(sub1 0) "-1")
(test-case '(sub1 (sub1 0)) "-2")
(test-case '(sub1 (sub1 2)) "0")

; zero?
(test-case '(zero? 0) "#t")
(test-case '(zero? 1) "#f")
(test-case '(zero? -1) "#f")
(test-case '(zero? #\t) "#f")
(test-case '(zero? #\f) "#f")

; not
(test-case '(not #f) "#t")
(test-case '(not #t) "#f")
(test-case '(not 0) "#f")

; null?
(test-case '(null? ()) "#t")
(test-case '(null? #f) "#f")

(test-case '(sub1 (add1 0)) "0")
(test-case '(add1 (sub1 0)) "0")

(test-case '(sub1 (add1 123456789)) "123456789")
(test-case '(add1 (sub1 123456789)) "123456789")

;; integer<->char
(test-case '(integer->char 65) "#\\A")
(test-case '(char->integer #\A) "65")
(test-case '(integer->char (add1 (char->integer #\l))) "#\\m")

;;; binary primitives
; +

(test-case '(+ 2 2) "4")
(test-case '(+ 0 0) "0")
(test-case '(+ -1000 1000) "0")
(test-case '(+ 2048 2048) "4096")
(test-case '(+ (+ (+ 1 2)
                  (+ 3 4))
               (+ (+ 5 6)
                  (+ 7 8))) "36")

; -
(test-case '(- 4 2) "2")
(test-case '(- 0 0) "0")
(test-case '(- 0 1000) "-1000")
(test-case '(- 4096 2048) "2048")
(test-case '(- (- (- 2048 1024)
                  (- 1024 512))
               (- (- 512 256)
                  (- 256 128))) "384")
