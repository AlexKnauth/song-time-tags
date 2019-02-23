#lang racket/base

(provide time-symbol-seconds
         seconds->time-symbol)

(require racket/format
         racket/match)

(define (time-symbol-seconds s)
  (match (symbol->string s)
    [(regexp #px"^(\\d+):(\\d+)$" (list _ m s))
     (+ (* 60 (string->number m)) (string->number s))]))

(define (seconds->time-symbol s)
  (define-values [q r] (quotient/remainder s 60))
  (string->symbol
   (format "~a:~a" q (~r r #:min-width 2 #:pad-string "0"))))

