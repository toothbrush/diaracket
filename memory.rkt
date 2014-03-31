#lang racket

(require (for-syntax diaracket/storage))

(provide (all-defined-out))

(define-syntax (storage-now stx)
  (syntax-case stx (storage-now rest taxo)
    [(storage-now rest)
     (with-syntax ([syms storage])
       #'(quote syms))]
     [(storage-now taxo)
     (with-syntax ([syms storage-taxo])
       #'(quote syms))]))


(define-syntax (remember stx)
  (syntax-case stx (remember rest taxo)
    [(remember sym rest)
     #'(begin (define-syntax _ (add-rest! (quote sym)))
              )]
    [(remember sym taxo)
     #'(begin (define-syntax _ (add-taxo! (quote sym)))
              )]))


(define ht (make-hash))
(define (setimpl k v) (hash-set! ht k v))
(define (emptyHash) (hash-map ht (lambda (k v) (hash-remove! ht k))) (void))
