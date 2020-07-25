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
; (test-case '8355840 "8355840")

(test-case '257 "257")
(test-case '4095 "4095")
(test-case '65535 "65535")
(test-case '-1 "-1")
(test-case '#t "#t")
(test-case '#f "#f")
(test-case '(add1 0) "1")
(test-case '(add1 (add1 0)) "2")
(test-case '(add1 (add1 -2)) "0")
