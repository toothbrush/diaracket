#lang s-exp "thermo-spec.rkt"

;; here we will provide the implementations of the
;; various components.

; import the available resources:
(taxonomy "taxo-impl.rkt")

;TODO oops, cannot do (implement ...) in REPL??
(implement ShowPicture ; controller
           (lambda (pic screenshow)
             (eval '(require net/http-client json))
             (eval '(define-values (status header response)
                      (http-sendrecv "httpbin.org" "/ip" #:ssl? 'tls)))
             (eval `(display-line "http thing? => " (read-json response)))
             (screenshow pic)
             ))

(implement ProcessPicture ; context
           (lambda (_button cameraGetPic publish nopublish)
             (let* ([pic (cameraGetPic)]
                    [dc  (new bitmap-dc% [bitmap pic])])
               ; do some fancy processing
               (send dc set-pen (make-pen #:width 5))
               (send dc set-brush (make-brush #:color (make-color 112 66 20 0.4)))
               (send dc draw-rounded-rectangle 
                     5   5   ; x y
                     116 116 ; w h
                     )
               (publish pic)
               )))

; let's have some ads as well. wonderful ads.

(implement ShowAd
           (lambda (str publish)
             (let* ([bmp (make-bitmap 300 100)]
                    [dc  (new bitmap-dc% [bitmap bmp])]
                    )
               (send dc draw-rectangle
                     0   10  ; Top-left at (0, 10), 10 pixels down from top-left
                     250 80) ; 30 pixels wide and 10 pixels high
               (send dc draw-text str
                     10  20)
               (publish bmp))))

(implement FetchAd
           (lambda (_button ip publish)
             (publish (~a "hello, this is ad #" (ip)))))

;; todo make IP device more convincing
;; todo keyword blacklisting, only for ctx, ctrs