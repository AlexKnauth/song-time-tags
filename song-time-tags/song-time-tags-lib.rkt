#lang racket

(provide song-names
         song-time-tags
         song-durations
         song-time-tags->entries
         group-and-sort-entries
         song-cross-reference-scores
         song-start-seconds
         song-start-seconds-table
         print-entry-groups
         print-cross-reference-scores
         plot-entry-groups
         frame-of-plot
         )

(require syntax/parse/define
         (for-syntax syntax/parse/class/paren-shape)
         plot
         (prefix-in pict/ (combine-in pict racket/draw))
         (prefix-in gui/ racket/gui/base)
         "util/commabrack.rkt"
         "util/play-sound.rkt"
         "util/time-symbol.rkt"
         "util/plot.rkt"
         "song-durations.rkt")

(begin-for-syntax
  (define-syntax-class song
    #:datum-literals [unquote]
    [pattern b:number
      #:with datum #'b]
    [pattern (a:number, b:number)
      #:with datum #'(a b)])

  (define-splicing-syntax-class who-list
    [pattern {~seq} #:with datum #'()]
    [pattern {~seq (who:commaseq)} #:with datum #'(who.norm ...)])

  (define-syntax-class time-tag
    #:datum-literals [-]
    [pattern (start-time:time - ~! end-time:time tag:id ... who:who-list)
      #:with datum
      #'[start-time.seconds end-time.seconds (tag ...) who.datum]]
    [pattern (start-time:time tag:id ... who:who-list)
      #:with datum
      #'[start-time.seconds start-time.seconds (tag ...) who.datum]]))

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

(struct entry [song start end tag who]
  #:transparent)

(define (song-time-tags->entries v)
  (for*/list ([p1 (in-list v)]
             [p2 (in-list (cdr p1))]
             [tag (in-list (third p2))])
    (entry (car p1) (first p2) (second p2) tag (fourth p2))))

;; -----------------------------------------------------------------------------

(define/match (list-of-number<? as bs)
  [['() '()] #false]
  [[(cons a as) (cons b bs)]
   (or (< a b)
       (and (= a b) (list-of-number<? as bs)))]
  [['() (cons _ _)] #true]
  [[(cons _ _) '()] #false])

(define (song->list-of-number s)
  (match s
    [(list (? number? a) (? number? b))  (list a b)]
    [(? number? b)                       (list 1 b)]))

(define/match (entry-key ent)
  [[(entry song (? number? start) _ _ _)]
   (append (song->list-of-number song) (list start))])

(define (group-and-sort-entries entries)
  (sort
    (group-by
      entry-tag
      (song-time-tags->entries entries))
    list-of-number<?
    #:key (compose entry-key last)))

;; -----------------------------------------------------------------------------

(define (song-start-seconds start-sec-tbl s)
  (hash-ref start-sec-tbl s))

(define (entry-seconds start-sec-tbl ent)
  (+ (song-start-seconds start-sec-tbl (entry-song ent))
     (entry-start ent)))

;; -----------------------------------------------------------------------------

(define (tag-cross-reference-score time-tags tag)
  (define (uses-tag? g)
    (for/or ([e (in-list (cdr g))])
      (member tag (third e))))
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
  (define tags-used (remove-duplicates (append-map third song-ttg)))
  (for/sum ([tag (in-list tags-used)])
    (tag-cross-reference-score time-tags tag)))

;; -----------------------------------------------------------------------------

(define (format-comma-seq lst)
  (apply ~a #:separator ", " lst))

(define (format-comma-list lst)
  (format "(~a)" (format-comma-seq lst)))

(define (format-song song)
  (cond
    [(list? song) (format-comma-list song)]
    [(number? song) (format "~a" song)]))

(define (format-who who)
  (match who
    [(? symbol? who) (~a who)]
    [(list (? symbol? f) (? symbol? as) ...)
     (format "~a[~a]" f (format-comma-seq as))]))

(define (format-who-list whos)
  (format-comma-list (map format-who whos)))

(define (print-entry-groups gs)
  (for ([g (in-list gs)])
    (define name (entry-tag (last g)))
    (printf "~a:\n" name)
    (for ([e (in-list g)])
      (match-define (entry s ta tb _ who) e)
      (printf "  ~a ~a - ~a ~a\n"
              (format-song s)
              (seconds->time-symbol ta)
              (seconds->time-symbol tb)
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
  (define start (hash-ref start-tbl song))
  (define dur (hash-ref dur-tbl song))
  (define end (+ start dur))
  (define name (cond [(list? song)  (~a (last song))]
                     [else          (format-song song)]))
  (render-named-time-interval start end name))

(define (find-entry-in-group song start group t)
  #;(for/last ([e (in-list group)]
             #:when (equal? song (entry-song e))
             #:when (<= (+ start (entry-start e))
                        t))
    e)
  (let loop ([es group])
    (match es
      ['() #false]
      [(cons e1 rst)
       #:when (not (equal? song (entry-song e1)))
       (loop rst)]
      [(cons e1 '())
       e1]
      [(cons e1 (cons e2 rstrst))
       #:when (not (equal? song (entry-song e2)))
       e1]
      [(cons e1 (and rst (cons e2 rstrst)))
       (cond
         [(<= (+ start (entry-start e2)) t)  (loop rst)]
         [else                               e1])])))

;; Entry -> Pict
(define (entry-info-pict ent)
  (match ent
    [(entry song t* _ phrase who)
     (define p
       (pict/vl-append
        (pict/text (format "~a ~a"
                           (format-song song)
                           (seconds->time-symbol t*)))
        (pict/text (~a phrase))
        (pict/text (format-who-list who))))
     (define bg
       (pict/filled-rounded-rectangle
        (+ (pict/pict-width p) 10)
        (+ (pict/pict-height p) 10)
        -0.2
        #:draw-border? #f #:color FloralWhite/α))
     (pict/cc-superimpose bg p)]))

(define (render-phrase-instance ent start phrase-i t)
  (cons
   (point-pict (vector t phrase-i)
               (entry-info-pict ent)
               #:anchor 'auto
               #:point-sym 'none)
   (render-phrase-time (+ start (entry-start ent)) phrase-i)))

(define (make-current-song-renderer songs dur-tbl start-tbl file-tbl n gs)

  (define held-playing? #false)
  (define (reset-held-playing!)
    (when held-playing?
      (send held-playing? stop)
      (set! held-playing? #false)))
  (define (start-play-phrase! ent)
    (match ent
      [(entry song t-start _ _ _)
       (define file
         (and file-tbl (hash-ref file-tbl song #false)))
       (when file
         (define vps
           (video-player-server/file file))
         (send vps seek t-start)
         (set! held-playing? vps))]
      [_ (void)]))

  (define (callback snip event x y)
    (define event-type (send event get-event-type))

    (cond
      [(and held-playing? x y (eq? event-type 'motion))
       (void)]
      [(and x y (memq event-type '(motion left-down)))
       (define song (get-current-song songs dur-tbl start-tbl x))

       (define phrase-i (exact-round y))
       (define phrase-exists? (and (<= 0 phrase-i) (< phrase-i n)))
       (define ent
         (and song phrase-exists?
              (find-entry-in-group song
                                   (hash-ref start-tbl song)
                                   (list-ref gs phrase-i)
                                   x)))
       (callback/overlay snip x song phrase-i phrase-exists? ent)
       (when (eq? event-type 'left-down)
         (callback/sound ent))]
      [else
       (send snip set-overlay-renderers #false)
       (reset-held-playing!)]))

  (define (callback/sound ent)
    (reset-held-playing!)
    (when ent
      (start-play-phrase! ent)))

  (define (callback/overlay snip x song phrase-i phrase-exists? ent)
    (define overlays
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
         [ent
          (render-phrase-instance ent (hash-ref start-tbl song) phrase-i x)]
         [else
          '()])))
    (send snip set-overlay-renderers overlays))

  callback)


(define (plot-entry-groups dim songs dur-sec-tbl file-tbl gs)
  (define n (length gs))
  (define start-sec-tbl (song-start-seconds-table songs dur-sec-tbl))
  (define phrases
    (for/list ([g (in-list gs)]) (entry-tag (last g))))
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
        (render-song-grid-lines start-sec-tbl)
        (render-phrase-grid-lines n)
        (for/list ([(g i) (in-indexed (in-list gs))])
          (points
           #:color i
           (for/list ([e (in-list g)])
             (list (entry-seconds start-sec-tbl e) i)))))))

    (send snip set-mouse-event-callback
          (make-current-song-renderer songs
                                      dur-sec-tbl
                                      start-sec-tbl
                                      file-tbl
                                      n
                                      gs))

    snip))

;; -----------------------------------------------------------------------------
