#lang racket

(provide (rename-out [module-begin #%module-begin]
                     [tuple-app #%app])
         (except-out (all-from-out racket) #%module-begin #%app)
         dimensions
         (all-from-out "song-time-tags-lib.rkt"))

(require syntax/parse/define
         (for-syntax syntax/parse/class/paren-shape)
         "song-time-tags-lib.rkt")

(define (dimensions w h) (list w h))

(define-syntax-parser tuple-app
  #:datum-literals [unquote]
  [(_ a:commaseq++) #'(list a.norm ...)]
  [(_ f:atom a:atom ...) #'(#%app f.norm a.norm ...)])

(begin-for-syntax
  (define-syntax-class dimensions
    #:literals [dimensions]
    [pattern {~and e (dimensions width height)}
      #:with id #'plot-dimensions
      #:with def #'(define id e)])

  (define-syntax-class song-names
    #:literals [song-names]
    [pattern {~and e (song-names . _)}
      #:with id #'names
      #:with def #'(define id e)])

  (define-syntax-class song-time-tags
    #:literals [song-time-tags]
    [pattern {~and e (song-time-tags . _)}
      #:with id #'time-tags
      #:with def #'(define id e)])

  (define-syntax-class song-durations
    #:literals [song-durations]
    [pattern {~and e (song-durations . _)}
      #:with id #'durations
      #:with def #'(define id e)])
  )

(define-simple-macro
  (module-begin
   {~alt {~once ds:dimensions}
         {~once ns:song-names}
         {~once tgs:song-time-tags}
         {~once drs:song-durations}}
   ...)
  #:with plot (datum->syntax this-syntax 'plot)
  (#%module-begin

   (provide plot)

   ds.def

   ns.def

   tgs.def
   (define sorted-grouped-entries
     (group-and-sort-entries tgs.id))
   (define songs (map first tgs.id))
   (define cross-reference-scores
     (song-cross-reference-scores tgs.id))

   drs.def

   ;; ------------------------------------------------------

   (define plot
     (plot-entry-groups ds.id songs drs.id sorted-grouped-entries))

   (define frame
     (frame-of-plot ds.id plot))

   (module+ main
     (print-entry-groups sorted-grouped-entries)
     ;(print-cross-reference-scores cross-reference-scores)
     (send frame show #true))
   ))

