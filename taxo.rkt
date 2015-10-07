;; here we'll declare some resources:

;; todo fix the fact that this is a syntactic include....
;; ..perhaps introduce #lang s-exp diaracket/taxonomy ???

;; note for the meantime that this file isn't independently loadable --
;; it is syntactically spliced into the top of the specification file.

(define-action Screen as Picture)

(define-source Camera as Picture)
(define-source Button as Boolean)

(define-source IP     as String)
(define-source Geo    as String)