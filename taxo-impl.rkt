;; in this file we will give the implementations of the 
;; taxonomy-defined resources.

;; note that it isn't independently loadable -- it is syntactically 
;; spliced into the thermo-impl.rkt file (because of the taxonomy-keyword)

(implement Screen ; int for now
           (lambda (bitmap)
             ;(display-line "Screen received " input)
             (let ([f (new frame% [label "Bitmap"])])
               (new message% [parent f] [label bitmap])
               (send f show #t))
             ))

(implement Camera
           (lambda ()
             (display-line "Returning a predefined image.")
             (read-bitmap "./sample.png")
             ))

(implement Button
           (lambda ()
             ; the button should always trigger
             #t
             ))




#|

#lang racket/gui

(define bitmap (read-bitmap "/tmp/red-arrow.bmp"))

(define f (new frame% [label "Bitmap"]))
(new message% [parent f] [label bitmap])
(send f show #t)

|#
