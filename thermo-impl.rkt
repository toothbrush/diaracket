#lang s-exp "thermo-spec.rkt"

;; here we will provide the implementations of the
;; various components.

; import the available resources:
(taxonomy "taxo-impl.rkt")

;TODO oops, cannot do (implement ...) in REPL??
(implement ShowPicture
           (lambda (pic screenshow)
             (dynamic-require 'net/http-client (void))
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
               ; do some fancy processing
               (publish pic)
               )))

; let's have some ads as well. wonderful ads.

(implement ShowAd
           (lambda (str publish)
             (let* ([bmp (make-bitmap 300 100)]
                    [dc  (new bitmap-dc% [bitmap bmp])]
                    )
               (send dc draw-rectangle
                     0  10  ; Top-left at (0, 10), 10 pixels down from top-left
                     250 80) ; 30 pixels wide and 10 pixels high
               (publish bmp))))

(implement FetchAd
           (lambda (_button getInternetInt pub)
             (pub "hello, this is ad #..")))

;; todo keyword blacklisting