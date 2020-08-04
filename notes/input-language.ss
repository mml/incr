;; see https://cisco.github.io/ChezScheme/csug9.5/summary.html#./summary:h0
#;(
(define ...)
(lambda (formals) body1 body2 ...)
(do ([var init update] ...)
  (test result
        ...)
  expr
  ...)
(let ([var val] ...) body1 body2 ...)
(let f ([var val] ...) body1 body2 ...)
let* letrec
if cond case and or
begin
'define-syntax 'let-syntax 'letrec-syntax 'syntax-rules 'syntax-case
quote unquote quasiquote unquote-splicing
set!
delay
)

#;(
vector make-vector make-string list
eq? eqv? equal? string=? string-ci=? char=? char-ci=?
vector->list list->vector number->string string->number symbol->string string->symbol char->integer integer->char string->list list->string)

#;(string? make-string string string-length string-ref string-set! string=? string-ci=? string<? string-ci<? string<=? string-ci<=? string>? string-ci>? string>=? string-ci>=? substring string-append string->list list>string string-copy string-fill!)

#;(char? char=? char-ci=? char<? char-ci<? char<=? char-ci<=? char>? char-ci>? char>=? char-ci>=? char-alphabetic? char-numeric? char-whitespace? char-upper-case? char-lower-case? char->integer integer->char char-upcase char-downcase)

#;(make-vector vector vector? vector-length vector-ref vector-set! vector->list list->vector vector-fill!)

#;(symbol->string string->symbol symbol?)

#;(pair? cons car cdr set-car! set-cdr! null? list? list length append reverse list-tail list-ref memq. memv. member assq assv assoc list->vector vector->list list->string string->list)

#;(boolean? pair? symbol? number? char? string? vector? port? procedure?)

#;(call-with-current-continuation (call/cc), values, call-with-values, dynamic-wind)

#;(eval scheme-report-environment null-environment interaction-environment (optional))

#;(display newline read write read-char write-char peek-char char-ready? eof-object? open-input-file open-output-file close-input-port close-output-port input-port? output-port? current-input-port current-output-port call-with-input-file call-with-output-file with-input-from-file(optional) with-output-to-file(optional))

#;(load (optional) transcript-on (optional) transcript-off (optional))

#;(force)

#;(procedure? apply map for-each)

#;(boolean? not)

#;(+ - * / abs quotient remainder modulo gcd lcm expt sqrt)
#;(numerator denominator rational? rationalize)
#;(floor ceiling truncate round)
#;(inexact->exact exact->inexact exact? inexact?)
#;(< <=  > >= =)
#;(zero? negative? positive? odd? even?)
#;(max min)
#;(sin cos tan asin acos atan)
#;(exp log)
#;(make-rectangular make-polar real-part imag-part magnitude angle complex?)
#;(number->string string->number)
#;(integer? rational? real? complex? number?)
