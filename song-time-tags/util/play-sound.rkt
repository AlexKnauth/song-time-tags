#lang racket/base

(provide video-player-server/file)

(require racket/class
         (only-in video/base clip)
         (only-in video/player video-player-server%))

;; A VideoPlayerServer has the methods:
;;  - play : -> Void
;;  - seek : PosReal -> Void
;;    where the PosReal is a time in seconds
;;  - stop : -> Void

;; PathString -> VideoPlayerServer
(define (video-player-server/file file)
  (new video-player-server% [video (clip file)]))

