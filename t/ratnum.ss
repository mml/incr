(module tests racket
  (provide runtests)
  (require "../s/test-driver.ss")
  (define (runtests)
    (test-cases "literals"
      ; Basic positive ratnum
      (test-case 2/3 "2/3")

      ; Negative numerator
      (test-case -2/3 "-2/3")

      ; Zero numerator
      (test-case 0/5 "0")

      ; Denominator of 1 (should return fixnum)
      (test-case 5/1 "5")

      ; GCD reduction in literal
      (test-case 6/8 "3/4")

      ; Already in lowest terms
      (test-case 7/11 "7/11")

      ; Negative with GCD
      (test-case -6/8 "-3/4")

      ; Larger prime fraction
      (test-case 355/113 "355/113")
      )

    (test-cases "Equality and identity"
      ; Identical literals - separate allocations, so not eq?
      (test-case (= 1/2 1/2) "#t")
      (test-case (eq? 1/2 1/2) "#f")

      ; Different literals reducing to same value
      (test-case (= 2/4 1/2) "#t")
      (test-case (eq? 2/4 1/2) "#f")

      ; Ratnum that reduces to fixnum vs fixnum literal
      (test-case (= 3/1 3) "#t")
      (test-case (eq? 3/1 3) "#t")

      ; Negative ratnum literals with same value
      (test-case (= -1/2 -1/2) "#t")
      (test-case (eq? -1/2 -1/2) "#f")

      ; Different GCD-reducible literals
      (test-case (= 6/8 3/4) "#t")
      (test-case (eq? 6/8 3/4) "#f")

      ; Zero ratnum
      (test-case (= 0/5 0) "#t")
      (test-case (eq? 0/5 0) "#t")

      ; Unequal ratnums
      (test-case (= 1/2 1/3) "#f")
      (test-case (= 2/5 3/7) "#f")
      )

    (test-cases "Division returning fixnums (exact division)"
      ; Basic exact division - returns fixnum
      (test-case
        (/ 6 2)
        "3")

      ; Exact division with negative numbers
      (test-case
        (/ -10 5)
        "-2")

      ; Exact division with both negative
      (test-case
        (/ -8 -2)
        "4")

      ; Division by 1 returns original
      (test-case
        (/ 7 1)
        "7")

      ; Division by -1
      (test-case
        (/ 6 -1)
        "-6")

      ; Zero divided by positive
      (test-case
        (/ 0 5)
        "0"))

    (test-cases "Division returning ratnums (inexact division)"
      ; Basic ratnum - 3/4
      (test-case
        (/ 3 4)
        "3/4")

      ; Ratnum with negative numerator
      (test-case
        (/ -3 4)
        "-3/4")

      ; Ratnum with negative denominator (sign in numerator)
      (test-case
        (/ 3 -4)
        "-3/4")

      ; Both negative (positive result)
      (test-case
        (/ -3 -4)
        "3/4")

      ; Larger ratnum
      (test-case
        (/ 7 11)
        "7/11"))

    (test-cases "GCD normalization"
      ; GCD reduces to lowest terms - (6 8) -> gcd=2 -> 3/4
      (test-case
        (/ 6 8)
        "3/4")

      ; GCD with larger numbers
      (test-case
        (/ 20 30)
        "2/3")

      ; GCD with prime numerator
      (test-case
        (/ 5 15)
        "1/3")

      ; Already in lowest terms
      (test-case
        (/ 7 9)
        "7/9"))

    (test-cases "Predicate tests"
      ; rational? true for fixnums
      (test-case
        (rational? 5)
        "#t")

      ; rational? false for non-numbers
      (test-case
        (rational? #t)
        "#f")

      ; number? true for fixnums
      (test-case
        (number? 10)
        "#t")

      ; exact? true for fixnums
      (test-case
        (exact? 7)
        "#t")

      ; integer? true for fixnums
      (test-case
        (integer? -3)
        "#t"))

    (test-cases "Accessor tests"
      ; numerator of fixnum is itself
      (test-case
        (numerator 5)
        "5")

      ; denominator of fixnum is 1
      (test-case
        (denominator 5)
        "1")

      ; numerator of negative fixnum
      (test-case
        (numerator -7)
        "-7")

      ; denominator of negative fixnum
      (test-case
        (denominator -7)
        "1")

      ; numerator of positive ratnum
      (test-case
        (numerator 1/2)
        "1")

      ; denominator of positive ratnum
      (test-case
        (denominator 1/2)
        "2")

      ; numerator of different positive ratnum
      (test-case
        (numerator 3/7)
        "3")

      ; denominator of different positive ratnum
      (test-case
        (denominator 3/7)
        "7")

      ; numerator of negative ratnum
      (test-case
        (numerator -1/3)
        "-1")

      ; denominator of negative ratnum
      (test-case
        (denominator -1/3)
        "3")

      ; numerator of ratnum that reduces to fixnum
      (test-case
        (numerator 6/2)
        "3")

      ; denominator of ratnum that reduces to fixnum
      (test-case
        (denominator 6/2)
        "1"))

    (test-cases "Comparison with less-than (<)"
      ; Basic ratnum to ratnum: 1/4 < 1/2
      (test-case
        (< 1/4 1/2)
        "#t")

      ; Basic ratnum to ratnum: 1/2 < 1/4 (false)
      (test-case
        (< 1/2 1/4)
        "#f")

      ; Different denominators: 1/3 < 1/2
      (test-case
        (< 1/3 1/2)
        "#t")

      ; Cross-product comparison: 2/3 < 3/4 (8/12 < 9/12)
      (test-case
        (< 2/3 3/4)
        "#t")

      ; Negative ratnum < zero
      (test-case
        (< -1/2 0)
        "#t")

      ; Zero < positive ratnum
      (test-case
        (< 0 1/2)
        "#t")

      ; Ratnum < fixnum
      (test-case
        (< 1/2 1)
        "#t")

      ; Fixnum < ratnum (false)
      (test-case
        (< 1 1/2)
        "#f")

      ; Both negative: -1 < -1/2
      (test-case
        (< -1 -1/2)
        "#t")

      ; Equal ratnums (not less than)
      (test-case
        (< 1/2 1/2)
        "#f"))

    (test-cases "Comparison with greater-equal (>=)"
      ; Basic ratnum to ratnum: 1/4 >= 1/2 (inverse of <)
      (test-case
        (>= 1/4 1/2)
        "#f")

      ; Basic ratnum to ratnum: 1/2 >= 1/4
      (test-case
        (>= 1/2 1/4)
        "#t")

      ; Different denominators: 1/3 >= 1/2
      (test-case
        (>= 1/3 1/2)
        "#f")

      ; Cross-product comparison: 2/3 >= 3/4
      (test-case
        (>= 2/3 3/4)
        "#f")

      ; Negative ratnum >= zero (false)
      (test-case
        (>= -1/2 0)
        "#f")

      ; Zero >= positive ratnum (false)
      (test-case
        (>= 0 1/2)
        "#f")

      ; Ratnum >= fixnum (false)
      (test-case
        (>= 1/2 1)
        "#f")

      ; Fixnum >= ratnum
      (test-case
        (>= 1 1/2)
        "#t")

      ; Both negative: -1 >= -1/2 (false)
      (test-case
        (>= -1 -1/2)
        "#f")

      ; Equal ratnums (equal counts as >=)
      (test-case
        (>= 1/2 1/2)
        "#t"))

    (test-cases "Comparison with greater-than (>)"
      ; Basic ratnum to ratnum: 1/4 > 1/2 (false)
      (test-case
        (> 1/4 1/2)
        "#f")

      ; Basic ratnum to ratnum: 1/2 > 1/4 (true)
      (test-case
        (> 1/2 1/4)
        "#t")

      ; Different denominators: 1/3 > 1/2 (false)
      (test-case
        (> 1/3 1/2)
        "#f")

      ; Cross-product comparison: 2/3 > 3/4 (false)
      (test-case
        (> 2/3 3/4)
        "#f")

      ; Negative ratnum > zero (false)
      (test-case
        (> -1/2 0)
        "#f")

      ; Zero > positive ratnum (false)
      (test-case
        (> 0 1/2)
        "#f")

      ; Ratnum > fixnum (false)
      (test-case
        (> 1/2 1)
        "#f")

      ; Fixnum > ratnum (true)
      (test-case
        (> 1 1/2)
        "#t")

      ; Both negative: -1 > -1/2 (false)
      (test-case
        (> -1 -1/2)
        "#f")

      ; Equal ratnums (not greater than)
      (test-case
        (> 1/2 1/2)
        "#f"))

    (test-cases "Comparison with less-equal (<=)"
      ; Basic ratnum to ratnum: 1/4 <= 1/2 (true)
      (test-case
        (<= 1/4 1/2)
        "#t")

      ; Basic ratnum to ratnum: 1/2 <= 1/4 (false)
      (test-case
        (<= 1/2 1/4)
        "#f")

      ; Different denominators: 1/3 <= 1/2 (true)
      (test-case
        (<= 1/3 1/2)
        "#t")

      ; Cross-product comparison: 2/3 <= 3/4 (true)
      (test-case
        (<= 2/3 3/4)
        "#t")

      ; Negative ratnum <= zero (true)
      (test-case
        (<= -1/2 0)
        "#t")

      ; Zero <= positive ratnum (true)
      (test-case
        (<= 0 1/2)
        "#t")

      ; Ratnum <= fixnum (true)
      (test-case
        (<= 1/2 1)
        "#t")

      ; Fixnum <= ratnum (false)
      (test-case
        (<= 1 1/2)
        "#f")

      ; Both negative: -1 <= -1/2 (true)
      (test-case
        (<= -1 -1/2)
        "#t")

      ; Equal ratnums (equal counts as <=)
      (test-case
        (<= 1/2 1/2)
        "#t"))

    #;(test-cases "Fixnum arithmetic with division"
      ; Chain of divisions
      (test-case
        (/ (/ 12 2) 3)
        "2")

      ; Division in arithmetic
      (test-case
        (+ (/ 1 2) (/ 1 2))
        "1"))

))
