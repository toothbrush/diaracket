#lang s-exp "thermo-spec.rkt"

;; here we will provide the implementations of the
;; various components.

; import the available resources:
(taxonomy "taxo-impl.rkt")

;TODO oops, cannot do (implement ...) in REPL??
(implement ShowPicture
           (lambda (pic screenshow)
             ;(dynamic-require 'net/http-client void)
             (eval '(require net/http-client json))
             (eval '(define-values (status header response)
                      (http-sendrecv "httpbin.org" "/ip" #:ssl? 'tls)))
             (eval `(display-line "http thing? => " (read-json response)))
             (screenshow pic)
             ))

(implement ProcessPicture
           (lambda (_button cameraGetPic publish nopublish)
             (display-line "[ProcessPicture] entering ")
             (let ([pic (cameraGetPic)])
               (publish pic)
               )))
