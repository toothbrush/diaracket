#lang racket

;; these lists will be used for compile-time recording
;; of the declared identifiers. then, an implementation file
;; can be analysed for validity, at transform time.

(define storage '())
(define storage-taxo '())

(define (add-rest! x) (set! storage (cons x storage)))
(define (add-taxo! x) (set! storage-taxo (cons x storage-taxo)))

(provide (all-defined-out))