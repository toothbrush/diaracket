#lang frtime
(require "useful.rkt")
(require frtime/animation
         frtime/gui)
(require racket/base)
(require racket/match)

; Definition of the component implementation
; Published sources are defined as event streams
;(define tick_impl (make-button "Tick"))

(define (temp_impl)
  (display-line "what's the temperature now? ")
  (let ([val (read)])
    (match (number? val)
      [#t   val]
      [else 0] ; default value
      ))  
  )

(define (fanOff_impl) (display-line "I am off"))

(define (tempwarningcontext_impl tick temp)(< (temp) 30))

(define (fancontroller_impl value action) (if value (action) void))

(define tickdesc
 (lambda () #t))

(define tick_impl ((changes seconds) . ==> . (lambda (x) (tickdesc))))

; Definition of the composition of the components as a composition of event streams
(define deploy ((tick_impl . ==> . (lambda (x) (tempwarningcontext_impl x temp_impl))) . ==> . (lambda (x) (fancontroller_impl x fanOff_impl))))

; Display of the interface
(display-shapes (list deploy))
