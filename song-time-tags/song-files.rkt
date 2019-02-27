#lang racket/base

(provide song-files itunes)

(require racket/match
         racket/path
         racket/string
         syntax/parse/define
         "util/commabrack.rkt"
         (for-syntax racket/base
                     syntax/parse))

(begin-for-syntax
  (define-syntax-class song
    #:datum-literals [unquote]
    [pattern track:number
      #:with datum #'track]
    [pattern (disk:number, track:number)
      #:with datum #'(disk track)]))

(define-syntax-parser song-files
  #:literals [itunes]
  ;; TODO: add more ways to specify where song files are
  [(_ {~and e (itunes . _)}) #'e])

(define-syntax-parser itunes
  [(_ #:album-artist album-artist:str
      #:album album:str
      (s:song name:str)
      ...)
   #'(let ([album-dir
            (itunes-album-dir #:album-artist 'album-artist
                              #:album 'album)])
       (and album-dir
            (itunes-album-songs/other
             album-dir
             '((s.datum . name) ...))))])

;; -----------------------------------------------

;; Path String ... -> [Maybe Path]
(define (build-path-dir/∃? base . subs)
  (define path (apply build-path base subs))
  (and (directory-exists? path) path))

;; -> [Maybe Path]
(define (find-itunes-music-dir)
  (define home-dir
    (find-system-path 'home-dir))
  (define music
    (or (build-path-dir/∃? home-dir "Music")
        (build-path-dir/∃? home-dir "My Music")))
  (and
   music
   (or (build-path-dir/∃? music "iTunes" "iTunes Music")
       (build-path-dir/∃? music "iTunes" "iTunes Media" "Music")
       (build-path-dir/∃? music "iTunes"))))

;; -----------------------------------------------

;; String -> String
(define (normalize-for-path s)
  (regexp-replaces s '([#rx":" "_"])))

;; #:album-artist String #:album String -> [Maybe Path]
(define (itunes-album-dir #:album-artist album-artist
                          #:album album)
  (define itunes-music (find-itunes-music-dir))
  (define album-dir
    (and itunes-music
         (build-path itunes-music
                     (normalize-for-path album-artist)
                     (normalize-for-path album))))
  (and album-dir (directory-exists? album-dir) album-dir))

;; Path -> [Hashof Song Path]
(define (itunes-album-songs album-dir)
  (define rx:disk-track #px"^(|(\\d+)-)(\\d+) ")
  (for/hash ([file (in-directory album-dir)]
             #:when (regexp-match?
                     rx:disk-track
                     (path->string (file-name-from-path file))))
    (match (path->string (file-name-from-path file))
      [(regexp rx:disk-track (list _ _ #f track))
       (values (string->number track)
               file)]
      [(regexp rx:disk-track (list _ _ disk track))
       (values (list (string->number disk)
                     (string->number track))
               file)])))

;; Path [Listof [Cons Song String]] -> [Hashof Song Path]
(define (itunes-album-songs/other album-dir other)
  (define initial (itunes-album-songs album-dir))
  (for/fold ([acc initial])
            ([p (in-list other)])
    (match-define (cons song name) p)
    (define file (find-song-by-name album-dir name))
    (cond [file (hash-set acc song file)]
          [else acc])))

;; Path String -> [Maybe Path]
(define (find-song-by-name dir name)
  (for/first ([file (in-directory dir)]
              #:when (string-contains?
                      (path->string (file-name-from-path file))
                      name))
    file))

