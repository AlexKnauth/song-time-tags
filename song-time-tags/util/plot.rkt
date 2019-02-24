#lang racket/base

(provide FloralWhite/α
         render-phrase
         render-phrase-time
         render-named-time-interval
         render-phrase-grid-lines
         render-song-grid-lines
         frame-of-plot)

(require racket/class
         racket/list
         racket/match
         plot
         (prefix-in pict/ (combine-in pict racket/draw))
         (prefix-in gui/ racket/gui/base)
         )

;; -----------------------------------------------------------------------------

;; Plotting the songs and phrases

(define FloralWhite (make-object pict/color% "FloralWhite"))
(define FloralWhite/α (make-object pict/color%
                        (send FloralWhite red)
                        (send FloralWhite green)
                        (send FloralWhite blue)
                        0.75))

;; Seconds Seconds String -> [Listof Renderer2D]
(define (render-named-time-interval start end name)
  (list
   (point-label (vector start -1) name
                #:anchor 'auto
                #:point-sym 'none)
   (rectangles (list (vector (ivl start end) (ivl -inf.0 +inf.0)))
               #:color "lightgray"
               #:alpha 0.1)))

;; Nat -> [Listof Renderer2D]
(define (render-phrase i)
  (list (hrule i #:width 0.2 #:color i)))

;; Seconds Nat -> [Listof Renderer2D]
(define (render-phrase-time t i)
  (list (vrule t #:width 0.2 #:color i)))

;; -----------------------------------------------------------------------------

;; Nat -> [Listof Renderer2D]
(define (render-phrase-grid-lines n)
  (for/list ([i (in-range n)])
    (hrule i #:width 0.05 #:color i)))

;; [Hashof Song Seconds] -> [Listof Renderer2D]
(define (render-song-grid-lines start-sec-tbl)
  (for/list ([(s start) (in-hash start-sec-tbl)])
    (define n (if (list? s) (last s) s))
    (vrule
     #:color (if (= n 1) "black" "gray")
     start)))

;; -----------------------------------------------------------------------------

;; Putting the Plot into a Gui Window

(define no-interactive-move-pasteboard%
  (class gui/pasteboard%
    (super-new)
    (define/augment (can-interactive-move? e) #false)))

(define (frame-of-plot dims plot)
  (match-define (list w h) dims)

  (define pasteboard (new no-interactive-move-pasteboard%))
  (send pasteboard insert plot)

  (define frame
    (new gui/frame%
         [label "plot"]
         [width (+ w 50)]
         [height (+ h 60)]))

  (define canvas
    (new gui/editor-canvas%
         [parent frame]
         [editor pasteboard]))

  frame)

;; -----------------------------------------------------------------------------
