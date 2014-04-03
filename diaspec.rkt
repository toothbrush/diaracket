#lang racket

(require diaracket/useful  (for-syntax diaracket/useful))
(require diaracket/structs (for-syntax diaracket/structs))
(require diaracket/memory  (for-syntax diaracket/memory))
(require (for-syntax racket/contract))
(require (for-syntax racket/list))

(provide (all-defined-out)
         (all-from-out diaracket/useful)
         (all-from-out diaracket/memory)
         (except-out (all-from-out racket) #%module-begin)
         (rename-out [specification-module-begin         #%module-begin]))

;todo solve issue where we must group declarations.
(define-syntax (process-spec-body stx)
  (syntax-case stx (define-action define-source define-context define-controller)
    [(_ 
      ((define-action     nma tya) ...)
      ((define-source     nmb tyb) ...)
      ((define-context    nmc tyc ctra) ...)
      ((define-controller nmd     ctrb) ...)
      )
     (with-syntax ([run (make-id "run" stx)]
                   [implementation-module-begin (make-id "module-begin-inner" stx)])
       #`(begin
           (clearList) ;; reset the defined-components list.
           
           ;; intro the definitions
           (intro-spec action     nma tya) ...
           (intro-spec source     nmb tyb) ...
           (intro-spec context    nmc tyc ctra) ...
           (intro-spec controller nmd     ctrb) ...
           
           ;; copy the architect's declarations
           (define-something action     nma tya) ...
           (define-something source     nmb tyb) ...
           (define-something context    nmc tyc ctra) ...
           (define-something controller nmd     ctrb) ...
           
           (require diaracket/structs)
           (require diaracket/useful)
           (require diaracket/fwexec)
           (require diaracket/memory)
           (require racket)
           
           (begin-for-syntax
             (remember nma taxo) ...
             (remember nmb taxo) ...
             (remember nmc rest) ...
             (remember nmd rest) ...
             )
           
           (define-syntax (implement sstx)
             (syntax-case sstx []
               [(_ name args (... ...))
                (with-syntax 
                    ([ins (make-id "implement-~a" sstx  #'name)])
                  (unless (ormap (lambda (x) (equal? x (syntax->datum #'name))) 
                                 (append (storage-now rest) (storage-now taxo)))
                    (raise-syntax-error 
                     (syntax->datum #'name) " is not defined in " #,(mymodname))
                    )
                  #'(begin (ins args (... ...)))
                  )]))
           
           (provide (all-defined-out)
                    run
                    (all-from-out diaracket/structs)
                    (all-from-out diaracket/useful)
                    (all-from-out diaracket/fwexec)
                    (all-from-out diaracket/diaspec)
                    (all-from-out diaracket/memory)
                    (except-out (all-from-out racket) #%module-begin)
                    (rename-out [implementation-module-begin   #%module-begin]))
           
           (define (run)
             (runfw (lambda (x) (lookupImplementation x)) ; delay evaluation of sysdescription till later.
                    (sysdescription)))
           
           (define-syntax (implementation-module-begin stx2)
             (syntax-case stx2 (implement taxonomy)
               [(_ (taxonomy f)
                   (implement decls (... ...)) (... ...)
                   )
                (with-syntax ([(taxo (... ...)) (datum->syntax stx2 (port->syntax
                                                                     (open-input-file 
                                                                      (syntax->datum #'f)) (list)))])
                  (check-presence-of-implementations 
                   (storage-now rest) #'((implement decls (... ...)) (... ...)))
                  #'(#%module-begin
                     (require diaracket/memory)
                     (emptyHash)
                     taxo (... ...) ; include syntax from taxo-file
                     (implement decls (... ...)) (... ...)
                     ))]))
           ))]))

;; this macro splices in the taxonomy specification files.
(define-syntax (specification-module-begin stx)
  (syntax-case stx (taxonomy define-action define-source define-context define-controller)
    [(_ 
      (taxonomy f)
      ((define-context    nmc tyc ctra) ...)
      ((define-controller nmd     ctrb) ...)
      )
     (with-syntax ([run (make-id "run" stx)]
                   [module-begin-inner (make-id "module-begin-inner" stx)]
                   [(taxo ...) (datum->syntax stx (port->syntax
                                                   (open-input-file 
                                                    (syntax->datum #'f)) (list)))])
       #`(#%module-begin
          (process-spec-body 
           taxo ...
           ((define-context    nmc tyc ctra) ...)
           ((define-controller nmd     ctrb) ...)
           )))]))

;; this procedure makes sure that all elements of required appear
;; in the list "provided", which should be of the form (listof (implement _name_ ...))
(define-for-syntax (check-presence-of-implementations required provided)
  (define provided-names '())
  (map
   (lambda (st)
     (syntax-case st (implement)
       [(_ a shiz ...) (set! provided-names (cons (syntax->datum #'a) provided-names))]
       ))
   (syntax-e provided))
  (map (lambda (req) (unless (ormap (lambda (candidate) (equal? candidate req)) provided-names)
                       (raise-syntax-error req ": component not implemented! use (implement ... )"))
         ) required))



;; bind the definitions, and add their contracts to a submodule
;; so that the implementation-submodules can still access them.
(define-syntax (contracts-module+ stx)
  (syntax-case stx []
    [(_ name obj)
     (with-syntax 
         ([contract-id (make-id "~a-contract" stx #'name)])
       #`(begin
           (define name obj)
           (provide name)
           (module+ contracts
             (require diaracket/structs)
             (require diaracket/useful)
             (define  contract-id (giveContract obj))
             (provide contract-id)
             )))]))


; this function basically translates the architect-provided
; declarations into the corresponding structs, then uses 
; contracts-module+ to bind them to predictable names. 
(define-syntax (intro-spec stx)
  (syntax-case stx (context controller source action do 
                            when-required when-provided get)
    [(_ context name ty [when-required get dr])
     #`(begin
         (contracts-module+ name (context 'name 
                                          (interactioncontract 
                                           'when-required (quoteDr dr) 'neverPublish) (quoteTy ty))))]
    [(_ context name ty [when-provided nm get dr pub])
     #`(begin
         (contracts-module+ name (context 'name 
                                          (interactioncontract 
                                           nm             (quoteDr dr) (quotePub pub)) (quoteTy ty))))]
    [(_ source name ty)
     #`(begin
         (contracts-module+ name (source 'name (quoteTy ty))))]
    [(_ action name ty)
     #`(begin 
         (contracts-module+ name (action 'name (quoteTy ty))))]
    [(_ controller name [when-provided nm do act])
     #`(begin
         (contracts-module+ name (controller 'name 
                                             (interactioncontract 
                                              nm           'none           act))))]))

; brr ugly, find module name.
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
  (syntax-case stx ()
    [(define-something type name rest ...)
     (with-syntax 
         ([structnm    (make-id "~a-structure" stx #'name)]
          [giveimp     (make-id "implement-~a" stx #'name)]
          [contract-id (make-id "~a-contract" stx #'name)]
          )
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
                     [require-spec 
                      (datum->syntax 
                       fstx
                       `(submod
                         "."
                         ,(make-id "~a-submodule" fstx #'name)) ; argh duplication. with-syntax isn't recursive
                       )]
                     [rs2 (datum->syntax
                           fstx
                           `(submod #,(mymodname) contracts))])
                  #'(begin
                      (module modname racket
                        (require diaracket/useful)
                        (require diaracket/structs (for-syntax diaracket/structs))
                        (require rs2)
                        (provide theimp)
                        (define/contract theimp contract-id f)
                        )
                      (require require-spec)
                      (setimpl 'name (structnm name theimp))
                      (display-line "Implementation " 'name " stored.")
                      )
                  )
                ]))
           (display-line "[" (~a #:min-width 10 (string-upcase (symbol->string 'type)))
                         "] => " "implement " 'name " :: "  (contract-name  (giveContract name)))
           (addDeviceToList name)))]))
