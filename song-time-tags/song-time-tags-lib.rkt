#lang racket

(provide song-names
         song-time-tags
         song-durations
         song-time-tags->entries
         group-and-sort-entries
         song-cross-reference-scores
         duration-symbol-seconds
         song-duration-seconds
         song-duration-seconds-table
         song-start-seconds
         song-start-seconds-table
         print-entry-groups
         print-cross-reference-scores
         plot-entry-groups
         frame-of-plot
         (for-syntax atom commaseq++)
         )

(require syntax/parse/define
         (for-syntax syntax/parse/class/paren-shape)
         plot
         (prefix-in pict/ (combine-in pict racket/draw))
         (prefix-in gui/ racket/gui/base))

(begin-for-syntax
  (define-syntax-class atom
    #:attributes [norm]
    #:datum-literals [unquote]
    [pattern {~and {~not {~or (unquote . _) {~brackets _ ...}}}
                   norm:expr}])

  (define-splicing-syntax-class brackapp
    [pattern {~seq f:atom {~brackets a:commaseq}}
      #:with norm #'(f.norm a.norm ...)]
    [pattern {~seq {~brackets f:atom a:commaseq}}
      #:with norm #'(f.norm a.norm ...)])

  (define-splicing-syntax-class commabrackapp
    #:datum-literals [unquote]
    [pattern {~seq ({~datum unquote} f:atom) {~brackets a:commaseq}}
      #:with norm #'(f.norm a.norm ...)]
    [pattern {~seq ({~datum unquote} {~brackets f:atom a:commaseq})}
      #:with norm #'(f.norm a.norm ...)])

  (define-splicing-syntax-class commaseq+
    #:attributes [[norm 1]]
    [pattern {~seq a:bexpr b:commabexpr ...}
      #:with [norm ...] #'(a.norm b.norm ...)])
  (define-splicing-syntax-class commaseq++
    #:attributes [[norm 1]]
    [pattern {~seq a:bexpr b:commabexpr ...+}
      #:with [norm ...] #'(a.norm b.norm ...)])
  (define-splicing-syntax-class commaseq
    #:attributes [[norm 1]]
    [pattern {~seq} #:with [norm ...] '()]
    [pattern {~seq :commaseq+}])

  (define-splicing-syntax-class bexpr
    #:attributes [norm]
    [pattern {~seq :brackapp}]
    [pattern {~seq :atom}])

  (define-splicing-syntax-class commabexpr
    #:attributes [norm]
    #:datum-literals [unquote]
    [pattern {~seq :commabrackapp}]
    [pattern {~seq ({~datum unquote} :atom)}]))

(begin-for-syntax
  (define-syntax-class song
    #:datum-literals [unquote]
    [pattern (a, b)
      #:with datum #'(a b)])

  (define-syntax-class time-tag
    [pattern (time tag:id ...)
      #:with datum #'[time (tag ...) ()]]
    [pattern (time tag:id ... (who:commaseq))
      #:with datum #'[time (tag ...) (who.norm ...)]]))

(define-simple-macro
  (song-names
    (song:song name:str)
    ...)
  (hash (~@ 'song.datum 'name) ...))

(define-simple-macro
  (song-time-tags
    (song:song
      time-tag:time-tag
      ...)
    ...)
  (quote
    ([song.datum . (time-tag.datum ...)] ...)))

(define-simple-macro
  (song-durations
    (song:song time)
    ...)
  (hash (~@ 'song.datum 'time) ...))

(define (song-time-tags->entries v)
  (for*/list ([p1 (in-list v)]
             [p2 (in-list (cdr p1))]
             [tag (in-list (second p2))])
    (list (car p1) (car p2) tag (third p2))))

;; -----------------------------------------------------------------------------

(define/match (list-of-number<? as bs)
  [['() '()] #false]
  [[(cons a as) (cons b bs)]
   (or (< a b)
       (and (= a b) (list-of-number<? as bs)))]
  [['() (cons _ _)] #true]
  [[(cons _ _) '()] #false])

(define (duration-symbol-seconds s)
  (match (symbol->string s)
    [(regexp #px"^(\\d+):(\\d+)$" (list _ m s))
     (+ (* 60 (string->number m)) (string->number s))]))

(define/match (entry-key ent)
  [[(list (list (? number? a) (? number? b))
          (? symbol? (app duration-symbol-seconds s))
          (? symbol?)
          (? list?))]
   (list a b s)])

(define (group-and-sort-entries entries)
  (sort
    (group-by
      third
      (song-time-tags->entries entries))
    list-of-number<?
    #:key (compose entry-key last)))

;; -----------------------------------------------------------------------------

(define (song-duration-seconds dur-tbl s)
  (duration-symbol-seconds (hash-ref dur-tbl s)))

(define (song-duration-seconds-table songs dur-tbl)
  (for/hash ([s (in-list songs)])
    (values s (song-duration-seconds dur-tbl s))))

(define (song-start-seconds songs dur-tbl s)
  (for/sum ([before (in-list songs)]
            #:break (equal? before s))
    (song-duration-seconds dur-tbl before)))

(define (song-start-seconds-table songs dur-sec-tbl)
  (for/fold ([start-sec-tbl (hash)]
             [t 0]
             #:result start-sec-tbl)
            ([s (in-list songs)])
    (define dur (hash-ref dur-sec-tbl s))
    (values (hash-set start-sec-tbl s t)
            (+ t dur))))

(define/match (entry-seconds/within-song ent)
  [[(list song
          (? symbol? (app duration-symbol-seconds s))
          (? symbol?)
          (? list?))]
   s])

(define (entry-seconds songs dur-tbl ent)
  (+ (song-start-seconds songs dur-tbl (first ent))
     (entry-seconds/within-song ent)))

;; -----------------------------------------------------------------------------

(define (tag-cross-reference-score time-tags tag)
  (define (uses-tag? g)
    (for/or ([e (in-list g)])
      (equal? (second e) tag)))
  ;; the number of songs it's used in
  (count uses-tag? time-tags))

(define (song-cross-reference-scores time-tags)
  (for/list ([g (in-list time-tags)])
    (define song (car g))
    (cons song (song-cross-reference-score time-tags song))))

(define (song-cross-reference-score time-tags song)
  ;; TODO: for each different tag used in that song
  ;;       tags used more than once only count once
  ;;       count the number of songs it's used in
  ;;       and add those together
  (define song-ttg (dict-ref time-tags song))
  (define tags-used (remove-duplicates (map second song-ttg)))
  (for/sum ([tag (in-list tags-used)])
    (tag-cross-reference-score time-tags tag)))

;; -----------------------------------------------------------------------------

(define (format-comma-seq lst)
  (apply ~a #:separator ", " lst))

(define (format-comma-list lst)
  (format "(~a)" (format-comma-seq lst)))

(define (format-song song)
  (format-comma-list song))

(define (format-who who)
  (match who
    [(? symbol? who) (~a who)]
    [(list (? symbol? f) (? symbol? as) ...)
     (format "~a[~a]" f (format-comma-seq as))]))

(define (format-who-list whos)
  (format-comma-list (map format-who whos)))

(define (print-entry-groups gs)
  (for ([g (in-list gs)])
    (define name (third (last g)))
    (printf "~a:\n" name)
    (for ([e (in-list g)])
      (match-define (list s t _ who) e)
      (printf "  ~a ~a ~a\n"
              (format-song s)
              t
              (format-who-list who)))
    (printf "\n")))

(define (print-cross-reference-scores xrs)
  (for ([xr (in-list xrs)])
    (define song (car xr))
    (define score (cdr xr))
    (printf "~a: ~a\n" (format-song song) score))
  (printf "\n"))

;; -----------------------------------------------------------------------------

;; Plotting the songs and phrases

(define FloralWhite (make-object pict/color% "FloralWhite"))
(define FloralWhite/α (make-object pict/color%
                        (send FloralWhite red)
                        (send FloralWhite green)
                        (send FloralWhite blue)
                        0.75))

(define (get-current-song songs dur-tbl start-tbl t)
  (for/or ([s (in-list songs)])
    (define start (hash-ref start-tbl s #f))
    (define dur (hash-ref dur-tbl s #f))
    (and start dur
         (let ()
           (define end (+ start dur))
           (and (<= start t)
                (< t end)
                s)))))

(define (render-song dur-tbl start-tbl song)
  (cond
    [song
     (define start (hash-ref start-tbl song))
     (define dur (hash-ref dur-tbl song))
     (define end (+ start dur))
     (list
      ;; TODO: It looks like `nonrenderer?` values aren't allowed here. Why not?
      #;(x-ticks (list (tick start #t (format-song song))))
      (point-label (vector start -1) (~a (second song))
                   #:anchor 'auto
                   #:point-sym 'none)
      (rectangles (list (vector (ivl start end) (ivl -inf.0 +inf.0)))
                  #:color "lightgray"
                  #:alpha 0.1))]
    [else
     '()]))

(define (render-phrase i)
  (list (hrule i #:width 0.2 #:color i)))

(define (render-phrase-instance song start dur phrase-i group t)
  (define entry
    (for/last ([e (in-list group)]
               #:when (equal? song (first e))
               #:when (<= (+ start (entry-seconds/within-song e))
                          t))
      e))
  (match entry
    [(list song t-sym phrase who)
     (define p
       (pict/vl-append
        (pict/text (format "~a ~a" (format-song song) t-sym))
        (pict/text (~a phrase))
        (pict/text (format-who-list who))))
     (define bg
       (pict/filled-rounded-rectangle
        (+ (pict/pict-width p) 10)
        (+ (pict/pict-height p) 10)
        -0.2
        #:draw-border? #f #:color FloralWhite/α))
     (list (vrule (+ start (duration-symbol-seconds t-sym))
                  #:width 0.2
                  #:color phrase-i)
           (point-pict (vector t phrase-i)
                       (pict/cc-superimpose bg p)
                       #:anchor 'auto
                       #:point-sym 'none))]
    [_
     '()]))

(define ((make-current-song-renderer songs phrases dur-tbl start-tbl gs)
         snip event x y)
  (define overlays
    (and x y (eq? (send event get-event-type) 'motion)
         (let ([song (get-current-song songs dur-tbl start-tbl x)]
               [phrase-i (exact-round y)])
           (define phrase-exists?
             (and (<= 0 phrase-i) (< phrase-i (length gs))))
           (append
            (cond
              [phrase-exists?
               (render-phrase phrase-i)]
              [else
               '()])
            (cond
              [song
               (render-song dur-tbl start-tbl song)]
              [else
               '()])
            (cond
              [(and song phrase-exists?)
               (render-phrase-instance song
                                       (hash-ref start-tbl song)
                                       (hash-ref dur-tbl song)
                                       phrase-i
                                       (list-ref gs phrase-i)
                                       x)]
              [else
               '()])))))
  (send snip set-overlay-renderers overlays))


(define (plot-entry-groups dim songs dur-tbl gs)
  (define n (length gs))
  (define dur-sec-tbl (song-duration-seconds-table songs dur-tbl))
  (define start-sec-tbl (song-start-seconds-table songs dur-sec-tbl))
  (define phrases
    (for/list ([g (in-list gs)]) (third (last g))))
  (parameterize
      ([plot-x-ticks (time-ticks)]
       [plot-y-ticks
        (ticks (λ (a b)
                 (for/list ([i (in-range n)])
                   (pre-tick i #true)))
               (λ (a b ps)
                 (for/list ([p (in-list ps)])
                   (define i (pre-tick-value p))
                   (cond [(< i 0) ""]
                         [(<= n i) ""]
                         [else (symbol->string (list-ref phrases i))]))))])
    (define snip
      (plot
       #:width (first dim)
       #:height (second dim)
       #:x-min -1
       #:x-max (add1 (for/sum ([s (in-list songs)])
                       (hash-ref dur-sec-tbl s)))
       #:y-min -1
       #:y-max (add1 n)
       (list
        (for/list ([s (in-list songs)])
          (vrule
           #:color (if (= (second s) 1) "black" "gray")
           (hash-ref start-sec-tbl s)))
        (for/list ([(g i) (in-indexed (in-list gs))])
          (list
           (hrule i #:width 0.05 #:color i)
           (points
            #:color i
            (for/list ([e (in-list g)])
              (list (entry-seconds songs dur-tbl e) i))))))))

    (send snip set-mouse-event-callback
          (make-current-song-renderer songs
                                      phrases
                                      dur-sec-tbl
                                      start-sec-tbl
                                      gs))

    snip))

;; -----------------------------------------------------------------------------

;; Putting the Plot into a Gui Window

(define (frame-of-plot dims plot)
  (match-define (list w h) dims)

  (define pasteboard (new gui/pasteboard%))
  (send pasteboard insert plot)

  (define frame
    (new gui/frame%
         [label "Hamilton Cross-References"]
         [width (+ w 50)]
         [height (+ h 60)]))

  (define canvas
    (new gui/editor-canvas%
         [parent frame]
         [editor pasteboard]))

  frame)

;; -----------------------------------------------------------------------------
