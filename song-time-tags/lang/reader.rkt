#lang s-exp syntax/module-reader song-time-tags/song-time-tags
#:read sweet-read
#:read-syntax sweet-read-syntax
#:info get-info
#:language-info #((submod sweet-exp language-info) get-language-info #f)

(require (submod sweet-exp/main link-reader))
(define-values (sweet-read sweet-read-syntax)
  (sweet-link read read-syntax))

(define (get-info key default f)
  (define (fallback) (f key default))
  (case key
    [(drracket:indentation)
     (dynamic-require 'sweet-exp/indent 'indent)]
    [else (fallback)]))

