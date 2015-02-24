; this is a static version of the spec, for trying typed racket.

(module warnfwspec racket
  
  (require (except-in "diaspec.rkt" #%module-begin))
  (require "useful.rkt")
  (require (for-syntax "useful.rkt"))
   
   (clearList)
   (begin
     (contracts-module+ fanspeed (action 'fanspeed 'int)))
   (begin
     (define-struct/contract
      fanspeed-impl
      ((spec any/c) (implem (giveContract fanspeed)))
      #:transparent)
     (provide (struct-out fanspeed-impl) implement-fanspeed)
     (define-syntax (implement-fanspeed fstx)
       (syntax-case fstx ()
         ((_ f)
          (with-syntax
           ((modname (make-id "~a-module" fstx #'fanspeed))
            (theimp (make-id "the-~a" fstx #'fanspeed))
            (require-spec
             (datum->syntax
              fstx
              `(submod "." ,(make-id "~a-module" fstx #'fanspeed))))
            (rs2
             (datum->syntax fstx `(submod "warnfwspec.rkt" contracts))))
           #'(begin
               (module modname typed/racket
                 (require "useful.rkt")
                 (require "data.rkt")
                 (require (for-syntax "data.rkt"))
                 (require rs2)
                 (provide theimp)
                 ; was
                 ;(define/contract theimp fanspeed-contract f)
                 (: theimp (Number -> String))
                 (define theimp f)
                 )
               (require require-spec)
               (setimpl 'fanspeed (fanspeed-impl fanspeed theimp))
               (display-line
                "Implementation for "
                'fanspeed
                " saved."))))))
     (display-line
      "["
      (string-upcase (symbol->string 'action))
      "] instantiate with, "
      "(implement "
      'fanspeed
      "...)"
      " :: "
      (contract-name (giveContract fanspeed)))
     (addDeviceToList fanspeed))
   (require "data.rkt")
   (require (for-syntax "useful.rkt"))
   (require "fwexec.rkt")

  (define-syntax (implement sstx)
     (syntax-case sstx ()
       ((_ name args ...)
        (with-syntax
         ((ins (make-id "implement-~a" sstx #'name)))
         (if (ormap
              (lambda (x) (equal? x (syntax->datum #'name)))
              (list 'fanspeed))
           #f
           (raise-syntax-error
            (syntax->datum #'name)
            " is not a defined component in "
            "warnfwspec.rkt"))
         #'(begin (ins args ...))))))
   (provide (all-defined-out)
            run
            (all-from-out "data.rkt")
            (all-from-out "useful.rkt")
            (all-from-out "fwexec.rkt")
  
            ; (except-out (all-from-out typed/racket) #%module-begin)
            (rename-out (module-begin-inner #%module-begin)))
   (define (run) (runfw (lambda (x) (deploy x)) (sysdescription)))
   (define-syntax (module-begin-inner stx2)
     (syntax-case stx2 ()
       ((_ decls ...)
        (with-syntax
         ()
         (check-presence-of-implementations (list 'fanspeed) #'(decls ...))
         #'(#%module-begin
     
            (emptyHash)
            decls
            ...)))))
  
  
  (define-for-syntax (check-presence-of-implementations required provided)
  (define provided-names '())
  (map
   (lambda (st)
   (syntax-case st (implement)
    [(_ a shiz ...) (set! provided-names (cons (syntax->datum #'a) provided-names))]
    ))
   (syntax-e provided))
  (map (lambda (req) (if (ormap (lambda (candidate) (equal? candidate req)) provided-names)
                         #t ; great,  provided
                         (raise-syntax-error req ": this component isn't implemented! use (implement ... )"))
         ) required))
  
  
  
  
  )
