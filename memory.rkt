#lang racket

(provide (for-syntax add-rest! add-taxo!))
(provide implementationsHash remember storage-now
         emptyHash setimpl)

(define (display-line . args)
  (for-each display args)
  (newline))

;; these will be used for compile-time recording
;; of the declared identifiers. then, an implementation file
;; can be analysed for validity, at transform time.
(begin-for-syntax 
  
  ; empty storage as a starting point
  (define storage      '())
  (define storage-taxo '())
  
  ; eventually these will be inserted, as syntax, into
  ; the generated modules.
  (define (add-rest! x) (set! storage (cons x storage)))
  (define (add-taxo! x) (set! storage-taxo (cons x storage-taxo)))
  )

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
     #'(begin (define-syntax _ (add-rest! (quote sym))))]
    [(remember sym taxo)
     #'(begin (define-syntax _ (add-taxo! (quote sym))))]))

(define implementationsHash
  (make-hash))
(define (setimpl k v) 
  (hash-set! implementationsHash k v))
(define (emptyHash)   
  (hash-map  implementationsHash 
             (lambda (k v) (hash-remove! implementationsHash k)))
  (void))

