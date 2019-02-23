#lang racket/base

(provide song-durations
         song-start-seconds-table
         (for-syntax time))

(require syntax/parse/define
         (for-syntax racket/base
                     "util/time-symbol.rkt"))

(begin-for-syntax
  (define-syntax-class song
    #:datum-literals [unquote]
    [pattern a:number
      #:with datum #'a]
    [pattern (a:number, b:number)
      #:with datum #'(a b)])

  (define-syntax-class time
    #:attributes [seconds]
    [pattern time-sym:id
      #:with seconds (time-symbol-seconds (syntax-e #'time-sym))])
  )

(define-simple-macro
  (song-durations
    (song:song time:time)
    ...)
  (hash (~@ 'song.datum 'time.seconds) ...))

;; -------------------------------------------------------------------

;; [Listof Song] [Hashof Song Seconds] -> [Hashof Song Seconds]
(define (song-start-seconds-table songs dur-sec-tbl)
  (for/fold ([start-sec-tbl (hash)]
             [t 0]
             #:result start-sec-tbl)
            ([s (in-list songs)])
    (define dur (hash-ref dur-sec-tbl s))
    (values (hash-set start-sec-tbl s t)
            (+ t dur))))

