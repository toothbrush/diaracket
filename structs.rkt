#lang racket

(require "useful.rkt")
(require (for-syntax "useful.rkt"))
(require (for-syntax racket/contract))
(provide (all-defined-out))

(define-struct/contract source
  ([name any/c]
   [type type?]
   ) #:transparent)

(define-struct/contract action
  ([name any/c]
   [type type?]
   ) #:transparent)

;; is the context's interaction contract reasonable? we also
;; check that if a context A has a datarequirement which is another
;; context B, that B is when-required.

(define icForContext?
  (lambda (ic)
    (and
     (interactioncontract? ic)
     (let ([acti (interactioncontract-activation ic)]
           [datareq (interactioncontract-datareq ic)]
           [pub (interactioncontract-publishoract ic)])
       (and
        (or (eq? acti 'when-required)
            (source?  acti)
            (context? acti))
        (or (eq? datareq 'none)
            (source?  datareq)
            (and (context? datareq)
                 (eq? (interactioncontract-activation (context-interactioncontract datareq))
                      'when-required))
            )
        (or (eq? pub 'alwaysPublish)
            (eq? pub 'maybePublish)
            (if (eq? acti 'when-required)
                (eq? pub 'neverPublish) #t)
            ))))))

(define icForController?
  (lambda (ic)
    (and
     (interactioncontract? ic)
     (context? (interactioncontract-activation ic))
     (eq? 'none (interactioncontract-datareq ic)) ;; we don't allow data requirements for controllers.
     (action? (interactioncontract-publishoract ic))
     )))

(define-struct/contract context
  ([name any/c]
   [interactioncontract icForContext?]
   [type type?]
   ) #:transparent)

(define (getName_activation obj)
  (cond
    [(source?  obj) (source-name obj)]
    [(context? obj) (context-name obj)]
    [else (error "Not an activation condition: " obj "!")]))

(define-struct/contract controller
  ([name any/c]
   [interactioncontract icForController?]
   ) #:transparent)


(define/contract
  (giveContractAct type)
  (-> type? contract?)
  (-> (interp type) void?)
  )

(define-syntax-rule (giveContractDev type)
  (-> (interp type)))

(define (outType obj)
  (cond
    [(source?  obj) (source-type  obj)]
    [(context? obj) (context-type obj)]
    ['none          'void]
    [else           (error obj " isn't a valid activation condition!")]))

(define (giveContractCtx c)
  (let*
      ([ic    (context-interactioncontract c)]
       [dr    (outType (interactioncontract-datareq ic))]
       [out   (interp (context-type c))]
       [activ (interactioncontract-activation ic)])
    (let ([input-term (case activ
                        ['when-required '()]
                        [else (list (interp (outType activ)))])]
          [dr-term    (case dr
                        ['void '()]
                        [else (list (-> (interp dr)))])]
          [publish-term (case activ
                          ['when-required '()]
                          [else (list (-> out void?))])]
          [nopublish-term (if (eq? (interactioncontract-publishoract ic) 
                                   'maybePublish)
                              (list (-> void?))
                              '())]
          [output-term (case activ
                         ['when-required (list out)]
                         [else           (list none/c)])] 
          ; TODO think about the impossible-contract being applicable, if a continuation should be used.
          ; realise that this doesn't change much: the blame is laid, but potential "damage" can already be
          ; done in the context. only useful thing is that if publish is never used, there's a complaint, 
          ; if only at runtime.
          )
    (eval #`#,(append (list #'->) 
                      input-term 
                      dr-term
                      publish-term
                      nopublish-term
                      output-term)))))


;;TODO add scribble docs everywhere.
;; todo add:
#|
(module* test racket/base
  (require (submod "..")
           rackunit)
  (check-true (true? #t))
  (check-false (true? #f))
  (check-exn exn:fail:contract? (lambda () (true? 3))))

;...tests etc
|#

(define (giveContractCtrl c)
  (let ([input  (context-type (interactioncontract-activation c))]
        [action (interactioncontract-publishoract c)])
    (-> (interp input) (giveContractAct (action-type action)) void?)
    ))

(define (getName obj)
  (apply 
   (cond
     [(source?     obj) source-name]
     [(context?    obj) context-name]
     [(controller? obj) controller-name]
     [(action?     obj) action-name]
     [else              (error obj " isn't an entity! \\in {ctx, ctrl, src, act}")]) obj))

(struct interactioncontract 
  (activation
   datareq
   publishoract) #:transparent)

(define-struct/contract diaspec
  ([sources      (non-empty-listof source?)]
   [contexts     (non-empty-listof context?)]
   [controllers  (non-empty-listof controller?)]
   [actions      (non-empty-listof action?)]
   ) #:transparent)

(define-syntax-rule (giveContract e)
  (cond 
    [(source?     e) (giveContractDev (source-type e))]
    [(action?     e) (giveContractAct (action-type e))]
    [(context?    e) (giveContractCtx e)]
    [(controller? e) (giveContractCtrl (controller-interactioncontract e))]
    [else (error "[ERR] Object (" e ") has no contract")]))

; Stateful list of defined devices.
(define specifiedDevices (list))
(define (addDeviceToList d)
  (set! specifiedDevices (cons d specifiedDevices)))
(define (clearList) 
  (set! specifiedDevices (list)))

(define (sysdescription)
  (make-diaspec
   (filter source?     specifiedDevices)
   (filter context?    specifiedDevices)
   (filter controller? specifiedDevices)
   (filter action?     specifiedDevices)))