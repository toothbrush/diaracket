#lang racket

(provide (all-defined-out))
(require diaracket/memory)

;; helper functions
(define (display-line . args)
  (for-each display args)
  (newline))

(define (type? t)
  (match t
    ['int  #t]
    ['bool #t]
    [else  (error "not a type! " t)]))

(define-syntax-rule (interp ty)
  (match ty
    ['int  number?]
    ['bool boolean?]
    ['void void?]
    [else (error "nonsense type! " ty)]))

(define make-id
  (case-lambda
    [(template stx id)
     (let ([str (format template (syntax->datum id))])
       (datum->syntax stx (string->symbol str)))]
    [(template stx)
     (datum->syntax stx (string->symbol template))]))

(define-syntax (quoteDr stx)
  (syntax-case stx (nothing)
    [(_ nothing) #''none]
    [(_ v)       #'v]))

(define-syntax (quotePub stx)
  (syntax-case stx (always_publish maybe_publish)
    [(_ always_publish) #''alwaysPublish]
    [(_ maybe_publish)  #''maybePublish]))

;; used for splicing a file into another, as syntax
(define (port->syntax p acc)
  (let* ([v (read p)])
    (cond
      [(eof-object? v)  acc]
      [else             (port->syntax p (append acc (list v)))])))

(define-syntax (quoteTy stx)
  (syntax-case stx (Boolean Integer)
    [(_ Boolean) #''bool]
    [(_ Integer) #''int]))

;; gets the implementation term
(define (lookupImplementation needle)
  (let ([str (hash-ref implementationsHash needle)])
    (vector-ref (struct->vector str) 2)))