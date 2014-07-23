#lang s-exp "evilcam-spec.rkt"

;; here we will provide the implementations of the
;; various components defined in the specification file referenced above

; import the implementations of the available resources from the taxonomy:
(taxonomy "taxo-impl.rkt")

;TODO oops, cannot do (implement ...) in REPL??
(implement ScreenController ; controller
           (lambda (pic screenshow)
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
                     5   5    ; x y
                     116 116) ; w h
               ;(eval (display-line "Eval is evil!"))
               (publish pic)
               )))

(implement DisplayApp
           (lambda (pic ad-str publish)
             (let* ([canvas (make-bitmap 450 450)]
                    [dc     (new bitmap-dc% [bitmap canvas])]
                    )
               (send dc draw-rectangle
                     0   10  ; Top-left at (0, 10), 10 pixels down from top-left
                     350 80) ; width and height 
               (send dc draw-text (ad-str)
                     10  20)
               (send dc draw-bitmap pic
                     10 100) ; superimpose the bmp
               (publish canvas))))

; let's have some ads as well. wonderful ads.

(implement MakeAd
           (lambda (ip) ; no publish function, because WhenRequired.
             (~a "showing Ad for IP " (ip))))
