#lang racket
(require redex)
(require rackunit)
(require pict)

(define-language diaspec
  [specification ::= (declaration ...)] ;; list of declarations
                 
  [declaration ::=
               (source     X as τ)
               (action     X as τ)
               (context    X as τ ctxt-interact)
               (controller X      ctrl-interact)]
  [τ  ::=
      Bool
      Int
      String
      Picture]
  [ctxt-interact ::=
                 [when provided Y getresource pub]
                 [when required   getresource]]
  [ctrl-interact ::=
                 [when provided Y do Z]]
  [getresource ::= 
               (get nothing)
               (get Z)]
  [pub ::=
       always-publish
       maybe-publish]
  [X Y Z ::=
     variable-not-otherwise-mentioned])

(define declaration?   (redex-match? diaspec declaration))
(define specification? (redex-match? diaspec specification))
 
(define e1
  (term (source Bla as Bool)))
(define e2
  (term (action Foo as Int)))
(define c1
  (term (context C1 as Bool [when provided Bla (get nothing) always-publish] )))
(define c2
  (term (context C2 as Picture [when required (get nothing)])))
(define c3
  (term (context C3 as Bool [when required (get C1)])))
(define c4
  (term (context C4 as Int [when provided S1 (get nothing) always-publish])))
(define c5
  (term (context C5 as Int [when provided S1 (get C2) always-publish])))
(define s1
  (term (,e1 ,c1)))
 
(module+ test
  ;; parsed terms:
  (test-equal (declaration?   e1) #true)
  (test-equal (declaration?   e2) #true)
  (test-equal (declaration?   c1) #true)
  (test-equal (declaration?   c2) #true)
  (test-equal (declaration?   c3) #true)
  (test-equal (declaration?   c4) #true)
  (test-equal (declaration?   c5) #true)
  (test-equal (specification? s1) #true)
  (test-equal (declaration? (term (source hello as Bool))) #true)
  ;; should be #f, since it's a nonsense term.
  (test-equal (declaration? (term Bool)) #false))

(define-extended-language diaspec+types
  diaspec
  [Γ ::= ((X : t) ...)]
  [t ::=
     (ACT τ)
     (SRC τ)
     (CTX-req τ)
     (CTX-prov τ)
     (CTRL)])
;; t is component type: tells us whether data flows in or out

(module+ test
  ;;; Here we'll test the per-declaration type judgment
  
  ;; lookup should fail here: => ill-typed term
  (test-equal (judgment-holds (⊢ ((y : (SRC Int))) (context C as Int [when provided yz
                                                                           (get nothing)
                                                                           maybe-publish]) t) t)
              '())

  (test-equal (judgment-holds (⊢ ((y : (SRC Int))) (context C as Int [when provided y
                                                                           (get nothing)
                                                                           maybe-publish]) t) t)
              '((CTX-prov Int)))
  (test-equal (judgment-holds (⊢ () ,e1 (SRC Bool))) #t)
  (test-equal (judgment-holds (⊢ () (source S1 as Picture) (SRC Picture))) #t)
  (test-equal (judgment-holds (⊢ () (context Cbad as Picture [when provided Nonexistent
                                                                   (get nothing) maybe-publish])
                                 t) t) '())
  ;; check that shadowing isn't allowed:
  (test-equal (judgment-holds (⊢ ((S1 : (SRC Int))) (source S1 as Picture) t) t) '())
  (test-equal (judgment-holds (⊢ ((unused : (SRC Int))) ,e2 (ACT Int))) #t)
  (test-equal (judgment-holds (⊢ () ,c2 (CTX-req Picture))) #t)
  (test-equal (judgment-holds (⊢ ((C1 : (SRC Int))) ,c3 (CTX-req Bool))) #t)
  (test-equal (judgment-holds (⊢ ((C1 : (CTX-req Int))) ,c3 (CTX-req Bool))) #t)
  (test-equal (judgment-holds (⊢ ((S1 : (SRC Int))) ,c4 (CTX-prov Int))) #t)
  (test-equal (judgment-holds (⊢ ((S1 : (CTX-prov Int))) ,c4 (CTX-prov Int))) #t)
  ;; subscription loop is illegal:
  (test-equal (judgment-holds (⊢ ((S1 : (CTX-prov Int)))
                                 (context S1 as Int (when provided S1 (get nothing) always-publish))
                                 t) t) '())
  (test-equal (judgment-holds (⊢ ((S1 : (CTX-prov Int)) (C2 : (CTX-req Int))) ,c5 (CTX-prov Int)))
              #t)
  (test-equal (judgment-holds (⊢ ((S1 : (CTX-prov Int)) (C2 : (SRC Int))) ,c5 (CTX-prov Int))) #t)
  (test-equal (judgment-holds (⊢ ((S1 : (CTX-prov Int)) (C2 : (CTX-prov Int))) ,c5 (CTX-prov Int)))
              #f)
  (test-equal (judgment-holds (⊢ ((S1 : (CTX-prov Int))
                                  (C2 : (CTX-prov Int))
                                  (A1 : (ACT Bool)))
                                 (controller Ctr1 [when provided C2 do A1]) (CTRL))) #t))

(module+ test
  ;; Here we'll test the type judgment which should return a new
  ;; binding entry for the context.
  
  (test-equal (judgment-holds (decl-ok () ,e1 (X : t)) (X : t))
              '((Bla : (SRC Bool))))
  (test-equal (judgment-holds (decl-ok () (source S1 as Picture) (X : t)) (X : t))
              '((S1 : (SRC Picture))))
  (test-equal (judgment-holds (decl-ok () (context Cbad as Picture [when provided Nonexistent
                                                                   (get nothing) maybe-publish])
                                       t) t)
              '())
  ;; check that shadowing isn't allowed:
  (test-equal (judgment-holds (decl-ok ((S1 : (SRC Int))) (source S1 as Picture) t) t)
              '())
  (test-equal (judgment-holds (decl-ok ((unused : (SRC Int))) ,e2 (X : t)) (X : t))
              '((Foo : (ACT Int))))
  (test-equal (judgment-holds (decl-ok () ,c2 (X : t) ) (X : t))
              '((C2 : (CTX-req Picture))))
  (test-equal (judgment-holds (decl-ok ((C1 : (SRC Int))) ,c3 (X : t) ) (X : t))
              '((C3 : (CTX-req Bool))))
  (test-equal (judgment-holds (decl-ok ((C1 : (CTX-req Int))) ,c3 (X : t) ) (X : t))
              '((C3 : (CTX-req Bool))))
  (test-equal (judgment-holds (decl-ok ((S1 : (SRC Int))) ,c4 (X : t) ) (X : t))
              '((C4 : (CTX-prov Int))))
  (test-equal (judgment-holds (decl-ok ((S1 : (CTX-prov Int))) ,c4 (X : t) ) (X : t))
              '((C4 : (CTX-prov Int))))
  ;; subscription loop is illegal:
  (test-equal (judgment-holds (decl-ok ((S1 : (CTX-prov Int)))
                                 (context S1 as Int (when provided S1
                                                      (get nothing)
                                                      always-publish)) t) t)
              '())
  (test-equal (judgment-holds (decl-ok ((S1 : (CTX-prov Int))
                                        (C2 : (CTX-req Int))) ,c5 (X : t)) (X : t))
              '((C5 : (CTX-prov Int))))
  (test-equal (judgment-holds (decl-ok ((S1 : (CTX-prov Int))
                                        (C2 : (SRC Int))) ,c5 (X : t)) (X : t))
              '((C5 : (CTX-prov Int))))
  (test-equal (judgment-holds (decl-ok ((S1 : (CTX-prov Int))
                                        (C2 : (CTX-prov Int))) ,c5 (X : t)) (X : t))
              '())
  (test-equal (judgment-holds (decl-ok ((S1 : (CTX-prov Int))
                                        (C2 : (CTX-prov Int))
                                        (A1 : (ACT Bool)))
                                       (controller Ctr1 [when provided C2 do A1]) (X : t)) (X : t))
              '((Ctr1 : (CTRL)))))

;; (⊢ Γ e t) – check that a declaration term is well-typed in a
;; context, return principal type.

;; TODO break up this judgment form into multiple blocks.

;; (\vdash gives ⊢ in Emacs, TeX input mode.)
(define-judgment-form diaspec+types
  #:mode (⊢ I I O)
  #:contract (⊢ Γ declaration t)
  [(side-condition (unique? X Γ))
   ----------------------- "intro-src"
   (⊢ Γ (source X as τ) (SRC τ))]
  [(side-condition (unique? X Γ))
   ----------------------- "intro-act"
   (⊢ Γ (action X as τ) (ACT τ))]
  [(side-condition (unique? X Γ))
   ----------------------- "ctx-req-get-ø"
   (⊢ Γ (context X as τ [when required (get nothing)]) (CTX-req τ))]
  [(where (SRC τ_2) (lookup Γ X_2))
   (side-condition (unique? X_1 Γ))
   ----------------------- "ctx-req-get-src"
   (⊢ Γ (context X_1 as τ_1 [when required (get X_2)]) (CTX-req τ_1))]
  [(where (CTX-req τ_2) (lookup Γ X_2))
   (side-condition (unique? X_1 Γ))
   ----------------------- "ctx-req-get-ctx"
   (⊢ Γ (context X_1 as τ_1 [when required (get X_2)]) (CTX-req τ_1))]
  [(where (SRC τ_2) (lookup Γ X_2))
   (side-condition (unique? X_1 Γ))
   ----------------------- "ctx-onSrc-get-ø"
   (⊢ Γ (context X_1 as τ_1 [when provided X_2 (get nothing) _]) (CTX-prov τ_1))]
  [(where (CTX-prov τ_2) (lookup Γ X_2))
   (side-condition (unique? X_1 Γ))
   ----------------------- "ctx-onCtx-get-ø"
   (⊢ Γ (context X_1 as τ_1 [when provided X_2 (get nothing) _]) (CTX-prov τ_1))]
  [(where (SRC τ_2) (lookup Γ X_2))
   (where (SRC τ_3) (lookup Γ X_3))
   (side-condition (unique? X_1 Γ))
   ----------------------- "ctx-onSrc-get-src"
   (⊢ Γ (context X_1 as τ_1 [when provided X_2 (get X_3) _]) (CTX-prov τ_1))]
  [(where (CTX-prov τ_2) (lookup Γ X_2))
   (where (SRC τ_3) (lookup Γ X_3))
   (side-condition (unique? X_1 Γ))
   ----------------------- "ctx-onCtx-get-src"
   (⊢ Γ (context X_1 as τ_1 [when provided X_2 (get X_3) _]) (CTX-prov τ_1))]
  [(where (SRC τ_2) (lookup Γ X_2))
   (where (CTX-req τ_3) (lookup Γ X_3))
   (side-condition (unique? X_1 Γ))
   ----------------------- "ctx-onSrc-get-ctx"
   (⊢ Γ (context X_1 as τ_1 [when provided X_2 (get X_3) _]) (CTX-prov τ_1))]
  [(where (CTX-prov τ_2) (lookup Γ X_2))
   (where (CTX-req τ_3) (lookup Γ X_3))
   (side-condition (unique? X_1 Γ))
   ;; REMARK: freshness is a stronger property than (different x1 x2)!
   ;; (side-condition (different X_1 X_2))
   ----------------------- "ctx-onCtx-get-ctx"
   (⊢ Γ (context X_1 as τ_1 [when provided X_2 (get X_3) _]) (CTX-prov τ_1))]
  [(where (CTX-prov τ_2) (lookup Γ X_2))
   (where (ACT τ_3) (lookup Γ X_3))
   (side-condition (unique? X_1 Γ))
   ----------------------- "intro-controller"
   (⊢ Γ (controller X_1 [when provided X_2 do X_3]) (CTRL))])

(define-judgment-form diaspec+types
  #:mode (decl-ok I I O)
  #:contract (decl-ok Γ declaration (X : t))
  [(⊢ Γ declaration t)
   (where X_new (varname declaration))
   -----------------------
   "decl-ok"
   (decl-ok Γ declaration (X_new : t))])

;; varname simply returns the bound name of a declaration.
(define-metafunction diaspec+types
  varname : declaration -> X
  [(varname (source X as any_1))         X]
  [(varname (action X as any_1))         X]
  [(varname (context X as any_1 any_2))  X]
  [(varname (controller X any_2))        X])

;; spec-ok simply does the type check then throws away the resulting
;; context. I.e., simply return yes-no answer for a specification.
(define-judgment-form diaspec+types
  #:mode (spec-ok I I)
  #:contract (spec-ok Γ specification)
  [(check-spec Γ specification Γ_2)
   -----------------------
   "type-erasure"
   (spec-ok Γ specification)])

;; check-spec type checks a specification and returns the context
;; containing bindings for each declaration.
(define-judgment-form diaspec+types
  #:mode (check-spec I I O)
  #:contract (check-spec Γ specification Γ)
  [-----------------------
   "empty-spec"
   (check-spec Γ_1 () Γ_1)]
  [(where (declaration_1 declaration_2 ...) specification)
   (decl-ok Γ declaration_1 (X : t))
   (check-spec (extend Γ (X : t)) (declaration_2 ...) Γ_2)
   -----------------------
   "check-spec"
   (check-spec Γ specification Γ_2)])

(module+ test
  (test-equal (judgment-holds
               (spec-ok ()
                        ((source S1 as Bool)))) #t)
  (test-equal (judgment-holds
               (check-spec ()
                        ((source S1 as Int)
                         (source S2 as Int)
                         (action A1 as Int)
                         (context C1 as Int (when provided S1 (get nothing) always-publish))
                         (controller Cc2 (when provided C1 do A1))) Γ) Γ)
              '(((Cc2 : (CTRL))
                 (C1 : (CTX-prov Int))
                 (A1 : (ACT Int))
                 (S2 : (SRC Int))
                 (S1 : (SRC Int))))))

(module+ test
  (test-equal (term (unique? y ((x : (CTRL)) (y : (SRC Int))))) #f)
  (test-equal (term (unique? z ((y : (SRC Int))))) #t))

; unique? tells us if a variable shadows another
(define-metafunction diaspec+types
  unique? : X Γ -> boolean
  [(unique? X ((X_1 : _) ... (X : _) (X_2 : _) ...)) #f]
  [(unique? X any) #t])

(module+ test
  (test-equal (term (extend () (x : (CTRL)))) (term ((x : (CTRL))))))
 
; (extend Γ (x : t) ...) add (x t) to Γ so that x is found before other x-s
(define-metafunction diaspec+types
  extend : Γ (X : t) ... -> Γ
  [(extend ((X_Γ : t_Γ) ...) (X : t) ...)
   ((X : t) ... (X_Γ : t_Γ) ...)])
 
(module+ test
  (test-equal (term (lookup ((x : (SRC Int)) (x : (CTRL)) (y : (ACT Picture))) x))
              (term (SRC Int)))
  (test-equal (term (lookup ((x : (SRC Int)) (x : (CTRL)) (y : (ACT Picture))) y))
              (term (ACT Picture)))
  (test-equal (term (lookup ((x : (SRC Int)) (x : (CTRL)) (y : (ACT Picture))
                             (z : (CTRL))) z))
              (term (CTRL))))
 
; (lookup Γ x) retrieves x's type from Γ
;; note: this function is fugly, sorry, but that's because escaping to
;; Scheme means i would have had to do (not (member x xs)) which
;; doesn't render prettily AT ALL. So that's why this is written in
;; pure Redex.
(define-metafunction diaspec+types
  lookup : Γ X -> t or #f
  ;; first case: found X at head of list!
  [(lookup ((X_1 : t) (X_2 : t_2) ...) X_1) t]
  ;; head ≠ X, recurse!
  [(lookup ((X_1 : t) (X_2 : t_2) ...) X_3) (lookup ((X_2 : t_2) ...) X_3)]
  ;; finally, cause lookup to fail:
  [(lookup () X) #f])

;; useful on black REPL
(define (col x)
  (colorize x "white"))

;(col (render-language diaspec))
;(col (render-language diaspec+types))

(define (paul/bracket open?)
  (if open?
      (text "⟦")
      (text "⟧")))

;; so Γ should never be italic.
(with-atomic-rewriter (term Γ) (lambda () (text "Γ" 'roman (default-font-size)))
  (begin
    (col (render-judgment-form ⊢))

    ;; (parameterize ([white-square-bracket homemade-white-square-bracket])
    (parameterize ([white-square-bracket paul/bracket])
      (parameterize ([judgment-form-cases (list 0 1)])
        (render-judgment-form ⊢))
      (parameterize ([judgment-form-cases (list 2 3 4 5 6 7 8 9 10)])
        (render-judgment-form ⊢))
      (parameterize ([judgment-form-cases (list 11)])
        (render-judgment-form ⊢))
      (col (render-judgment-form decl-ok))
      (col (render-judgment-form spec-ok))
      (col (render-judgment-form check-spec))
      (col (render-metafunctions lookup varname extend unique? #:contract? #t)))

    (render-language diaspec         "diaspec-grammar.pdf")
    (render-language diaspec+types   "diaspectypes-grammar.pdf")
    (parameterize ([white-square-bracket paul/bracket])
      (parameterize ([judgment-form-cases (list 0 1)])
        (render-judgment-form ⊢          "judgments-rules-src-act.pdf"))
      (parameterize ([judgment-form-cases (list 2 3 9 10)])
        (render-judgment-form ⊢          "judgments-rules-contexts.pdf"))
      (parameterize ([judgment-form-cases (list 11)])
        (render-judgment-form ⊢          "judgments-rules-controller.pdf"))
      ;; for complete output in the Appendix
      (parameterize ([judgment-form-cases (list 0 1 2 3 4 5 6)])
        (render-judgment-form ⊢          "judgments-rules-part1.pdf"))
      (parameterize ([judgment-form-cases (list 7 8 9 10 11)])
        (render-judgment-form ⊢          "judgments-rules-part2.pdf"))
      (render-judgment-form decl-ok    "decl-ok-rules.pdf")
      (render-judgment-form spec-ok    "spec-ok-rules.pdf")
      (render-judgment-form check-spec "check-spec-rules.pdf")

      (render-metafunction varname     "fn:varname.pdf"  #:contract? #t)
      (render-metafunction lookup      "fn:lookup.pdf"   #:contract? #t)
      (render-metafunction extend      "fn:extend.pdf"   #:contract? #t)
      (render-metafunction unique?     "fn:unique.pdf"   #:contract? #t)

      (render-metafunctions varname lookup extend unique?
                            ;; note that in a future version of Racket the
                            ;; keyword will be #:file
                            #:filename "all-metafunctions.pdf"
                            #:contract? #t))))

(module+ test
  (test-results))
