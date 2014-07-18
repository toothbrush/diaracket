#lang racket

(require "useful.rkt"  (for-syntax "useful.rkt"))
(require "structs.rkt" (for-syntax "structs.rkt"))
(require "memory.rkt"  (for-syntax "memory.rkt"))
(require (for-syntax racket/contract))
(require (for-syntax racket/list racket/match))

(provide (all-defined-out)
         (all-from-out "useful.rkt")
         (all-from-out "memory.rkt")
         (except-out (all-from-out racket)       #%module-begin)
         (rename-out [specification-module-begin #%module-begin]))

(define-syntax (process-spec-body stx)
  (syntax-case stx ()
    [(_ 
      (define-keyword nm args ...) ...)
     (with-syntax ([run (make-id "run" stx)]
                   [implementation-module-begin (make-id "module-begin-inner" stx)])
       #`(begin
           (clearList) ;; reset the defined-components list.
           
           ;; intro the definitions
           (append-contracts-module define-keyword nm args ...) ...
           
           ;; copy the architect's declarations
           (define-something        define-keyword nm args ...) ...
           
           ; pre-empt the contract which checks the description's validity
           (let ([dummy (sysdescription)]) (display-line "System seems reasonable."))
           
           (require "structs.rkt")
           (require "useful.rkt")
           (require "fwexec.rkt")
           (require "memory.rkt")
           (require racket)
           
           (begin-for-syntax
             (let ([where (match 'define-keyword
                                    ['define-action  #'taxo]
                                    ['define-source #'taxo]
                                    ['define-context #'rest]
                                    ['define-controller #'rest]
               )])
               (eval #`(remember nm #,where))
               ;(display-line "storage == " (eval #`(storage-now #,where)))
               ) ... )
                 
           
             
           
           (define-syntax (implement sstx)
             (syntax-case sstx []
               [(_ name as (... ...))
                (with-syntax 
                    ([ins (make-id "implement-~a" sstx  #'name)])
                  (unless (ormap (lambda (x) (equal? x (syntax->datum #'name))) 
                                 (append (storage-now rest) (storage-now taxo)))
                    (raise-syntax-error 
                     (syntax->datum #'name) " is not defined in " #,(mymodname))
                    )
                  #'(begin (ins as (... ...)))
                  )]))
           
           (provide (all-defined-out)
                    run
                    (all-from-out "structs.rkt")
                    (all-from-out "useful.rkt")
                    (all-from-out "fwexec.rkt")
                    (all-from-out "diaspec.rkt")
                    (all-from-out "memory.rkt")
                    (except-out (all-from-out racket) #%module-begin)
                    (rename-out [implementation-module-begin   #%module-begin]))
           
           ; delay evaluation of lookupImplementation till later.
           (define (run)
             (runfw (lambda (x) (lookupImplementation x))
                    (sysdescription)))
           
           (define-syntax (implementation-module-begin stx2)
             (syntax-case stx2 (implement taxonomy)
               [(_ (taxonomy f)
                   (implement decls (... ...)) (... ...))
                (with-syntax ([(taxo (... ...)) (datum->syntax stx2 (port->syntax
                                                                     (open-input-file 
                                                                      (syntax->datum #'f)) (list)))])
                  (check-presence-of-implementations 
                   (storage-now rest) #'((implement decls (... ...)) (... ...)))
                  #'(#%module-begin
                     (require "memory.rkt")
                     (emptyHash)
                     taxo (... ...) ; include syntax from taxo-file
                     (implement decls (... ...)) (... ...)
                     ))]))
           ))]))

;; this macro splices in the taxonomy specification files.
(define-syntax (specification-module-begin stx)
  (syntax-case stx (taxonomy)
    [(_ 
      (taxonomy f)
      (define-keyword nm args ...) ...)
     (with-syntax ([run (make-id "run" stx)]
                   [module-begin-inner (make-id "module-begin-inner" stx)]
                   [(taxo ...) (datum->syntax stx (port->syntax
                                                   (open-input-file 
                                                    (syntax->datum #'f)) (list)))])
       #`(#%module-begin
          (process-spec-body 
           taxo ...
           (define-keyword nm args ...) ...
           )))]))

;; this procedure makes sure that all elements of required appear
;; in the list "provided", which should be of the form (listof (implement _name_ ...))
(define-for-syntax (check-presence-of-implementations required provided)
  (define provided-names '())
  (map
   (lambda (st)
     (syntax-case st ()
       [(_ a _) (set! provided-names (cons (syntax->datum #'a) provided-names))]
       )) (syntax-e provided))
  (map (lambda (req) 
         (display-line "checking " req)
         ; TODO give a list of errors, don't stop at the first one?
         (unless (ormap (lambda (candidate) (equal? candidate req)) provided-names)
                       (raise-syntax-error req ": component not implemented! use (implement ... )"))
         ) required))

;; bind the definitions, and add their contracts to a submodule
;; so that the implementation-submodules can still access them.
;; module+ adds to module called $id$ if it already exists.
(define-syntax (contracts-module+ stx)
  (syntax-case stx []
    [(_ name obj)
     (with-syntax 
         ([contract-id (make-id "~a-contract" stx #'name)])
       #`(begin
           (define name obj)
           (provide name)
           (module+ contracts
             (require "structs.rkt")
             (require "useful.rkt")
             (define  contract-id (giveContract obj))
             (provide contract-id)
             )))]))


; this function basically translates the architect-provided
; declarations into the corresponding structs, then uses 
; contracts-module+ to bind them to predictable names. 
(define-syntax (append-contracts-module stx)
  (syntax-case stx (define-context define-controller define-source define-action do 
                     when-required when-provided get)
    [(_ define-context name ty [when-required get dr])
     #`(begin
         (contracts-module+ name 
                            (context 'name 
                                     (interactioncontract 
                                      'when-required (quoteDr dr) 'neverPublish) (quoteTy ty))))]
    [(_ define-context name ty [when-provided nm get dr pub])
     #`(begin
         (contracts-module+ name 
                            (context 'name 
                                     (interactioncontract 
                                      nm             (quoteDr dr) (quotePub pub)) (quoteTy ty))))]
    [(_ define-source name ty)
     #`(begin
         (contracts-module+ name (source 'name (quoteTy ty))))]
    [(_ define-action name ty)
     #`(begin 
         (contracts-module+ name (action 'name (quoteTy ty))))]
    [(_ define-controller name [when-provided nm do act])
     #`(begin
         (contracts-module+ name (controller 'name 
                                             (interactioncontract 
                                              nm           'none           act))))]))

; brr ugly, find module name.
; used to give nicer error messages, and to include submodules
(define-for-syntax (mymodname) 
  (last
   (regexp-split #rx"/"
                 ((lambda (x)
                    (if (symbol? x)
                        "./."
                        (path->string x)))
                  (resolved-module-path-name 
                   (if (current-module-declare-name) 
                       (current-module-declare-name)
                       (make-resolved-module-path 'diaspec)))))))

;define-syntax is necessary since we need to introduce identifiers.
;; this procedure gets called when a developer uses the (define-{context,..} x ..) macros
;; it instantiates the (implement-x ... ) macro, too.
(define-syntax (define-something stx)
  (syntax-case stx (define-something)
    [(define-something type name rest ...)
     (with-syntax 
         ([structnm    (make-id "~a-structure" stx #'name)]
          [giveimp     (make-id "implement-~a" stx #'name)]
          [contract-id (make-id "~a-contract"  stx #'name)])
       #`(begin
           (define-struct/contract structnm
             ([spec   any/c]
              [implem (giveContract name)]
              ) #:transparent )
           (provide (struct-out structnm) giveimp)
           (define-syntax (giveimp fstx)
             (syntax-case fstx ()
               [(_ f)
                (with-syntax 
                    ([modname   (make-id "~a-submodule" fstx #'name)]
                     [theimp    (make-id "~a-implementation" fstx #'name)]
                     [rs2 (datum->syntax
                           fstx
                           `(submod #,(mymodname) contracts))])
                  #`(begin
                      (module modname racket/gui
                        (require "useful.rkt")
                        (require "structs.rkt" (for-syntax "structs.rkt"))
                        (require rs2)
                        
                        ; IF DEVICE then provide.
                        #,(cond [(equal? 'type 'define-source)
                               #'(require net/http-client json)]
                              )
                        (provide theimp)
                        (define/contract theimp contract-id f))
                      (require #,(datum->syntax fstx `(submod "." ,#'modname)))
                      (setimpl 'name (structnm name theimp))
                      (display-line "Implementation " 'name " stored.")
                      ))]))
           (display-line "[" (~a #:min-width 10 (string-upcase (symbol->string 'type)))
                         "] => " "implement " 'name " :: "  (contract-name  (giveContract name)))
           (addDeviceToList name)))]))
