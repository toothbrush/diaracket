;; in this file we will give the implementations of the 
;; taxonomy-defined resources.

;; note that it isn't independently loadable -- it is syntactically 
;; spliced into the thermo-impl.rkt file (because of the taxonomy-keyword)

(implement Screen ; int for now
           (lambda (bitmap)
             ;(display-line "Screen received " input)
             (let ([f (new frame% [label "Bitmap"])])
               (new message% [parent f] [label bitmap])
               (send f show #t))
             ))

(implement Camera
           (lambda ()
             (display-line "Returning a predefined image.")
             (read-bitmap "./sample.jpg")
             ))

(implement Button
           (lambda ()
             ;placeholder: the button is pushed
             #t
             ))

(implement IP
           (lambda ()
     
             (define-values (status header response)
                      (http-sendrecv "httpbin.org" "/ip" #:ssl? 'tls))
             (let ([resp (read-json response)])
               (display-line "http thing? => " resp)
               (hash-ref resp 'origin "oops")
               )
             )) ;; return some value from the web
          