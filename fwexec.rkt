#lang racket
;;; The example implementation of a framework which takes the interaction description from
;;; the "warnfw" module and polls the sources, etc.

(require diaracket/diaspec)
(require diaracket/useful)
(require diaracket/structs)
(require (prefix-in c: racket/control))
(provide runfw)

;; handle subscribed controllers.
(define (controllersFor name val deployment sysdesc)
  (for-each (lambda (ctrl)
              (let ([ic (controller-interactioncontract ctrl)]
                    [nm (controller-name ctrl)])
                (cond [(equal? (getName_activation (interactioncontract-activation ic))
                               name)
                       (display-line "[ctrl] " nm " subscribed to <" name ">.")
                       (let ([ctrlVal (deployment nm)]
                             [act     (deployment (action-name 
                                                   (interactioncontract-publishoract ic)))])
                         (ctrlVal val act))])))
            (diaspec-controllers sysdesc)))

;; deal with contexts after a value-publish.
(define (contextsFor name val deployment sysdesc)
  (for-each 
   (lambda (ctx)
     (let* ([ic (context-interactioncontract ctx)]
            [activation (interactioncontract-activation ic)])
       (cond [(and ;; activation is when-req, so skip (these never react to publishes).
                   (not (equal? activation
                                'when-required))
                   ;; and subscribed to the component that's publishing
                   (equal? (getName_activation activation)
                           name))
              (fireContext ctx name val deployment sysdesc)
              ])))
   (diaspec-contexts sysdesc)))

(define (fireContext ctx name val deployment sysdesc)
  (let* ([ic            (context-interactioncontract ctx)]
         [nm            (context-name ctx)]
         [dr            (interactioncontract-datareq ic)]
         [dr-term       (match dr
                          ['none '()]
                          [else  `(,(lambda () (pullValue nm deployment dr)))])]
         [publishSpec   (interactioncontract-publishoract ic)]
         [ctx_impl      (deployment nm)]
         [nopub-term    (case publishSpec
                          ['maybePublish   `(,(lambda () 
                                                (display-line "[ctx ] " nm ": no publish") 
                                                (c:abort (void))))]
                          ['alwaysPublish  '()])]
         [cont     (lambda (ctx_val)
                     (display-line "[ctx ] " nm ": publishes " ctx_val)
                     (contextsFor    nm ctx_val deployment sysdesc)
                     (controllersFor nm ctx_val deployment sysdesc))]
         [call     (lambda (v) (append `(,val)
                                       dr-term
                                       `(,v) 
                                       nopub-term))])
    (display-line "[ctx ] " nm " subscribed to <" name ">.")
    (display-line "[ fw ] querying [ctx] " nm)
    (c:prompt (cont (c:control v (apply ctx_impl (call v)))) (c:abort (void)))
    ))


(define (pullValue asker deployment src/ctx)
  (let ([nm (getName_activation src/ctx)])
    (display-line "-- [ctx ] " asker " pulls " nm)
    (display-line " =>[ fw ] querying " nm)
    (let ([val (cond
                 [(source?  src/ctx)  ((deployment nm))]  ;; return this, the value.
                 [(context? src/ctx) 
                  (let ([concrete_ctx (deployment nm)]
                        [ic (context-interactioncontract src/ctx)])
                    (cond
                      [(eq? 'when-required (interactioncontract-activation ic))
                       (let* ([dr (interactioncontract-datareq ic)])
                         (cond
                           [(eq? dr 'none) (concrete_ctx)] 
                           ;; no data requirement, just pull the value. This
                           ;; could mean e.g. a context returning a static value.
                           [else           (concrete_ctx (lambda () (pullValue nm deployment dr)))]
                           ; fetch data requirement recursively.
                           ))
                       ]
                      [else (error "Context to be pulled (" nm 
                                   ") lacks WhenRequired contract!")]))]
                 [else (error "Hm, src/ctx is neither src nor ctx! " src/ctx)])])
    (display-line "   [ fw ] " nm " returns: " val)
    val)))

(define/contract (runfw deployment sysdesc) (-> procedure? diaspec? void?)
  (for-each (lambda (source) ; poll all sources
              ;; broadcast source value to all subscribed.
              (let* ([nm     (source-name source)]
                     [theSrc (deployment nm)])
                (display-line "[ fw ] querying [src] " nm)
                (let ([val (theSrc)])
                  (display-line "[src ] " nm " returns: " val)
                  (contextsFor nm val deployment sysdesc))))
            (diaspec-sources sysdesc)))
