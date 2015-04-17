#lang s-exp "webcamspec.rkt"

;; here we will provide the implementations of the
;; various components defined in the specification file referenced above

;; import the implementations of the available resources from the taxonomy:
(taxonomy "taxo-impl.rkt")

(implement Display ; controller
           (lambda (pic screenshow)
              (screenshow pic)))

;;(implement IP (lambda () "muahahaha")) ; <- impossible to reimplement taxo items

(implement ProcessPicture ; context
           (lambda (_button cameraGetPic publish)
             (let* ([pic (cameraGetPic)]
                    [dc  (new bitmap-dc% [bitmap pic])])
               
               ; do some fancy processing
               (send dc set-pen   (make-pen #:width 5))
               (send dc set-brush (make-brush #:color (make-color 112 66 20 0.4)))
               (send dc draw-rounded-rectangle 
                     5   5    ; x y
                     116 116) ; w h
               (publish pic))))


(implement ComposeDisplay
           (lambda (pic get-ad-text publish nopublish)
             (let* ([canvas (make-bitmap 450 450)]
                    [adTxt  (get-ad-text)]
                    [dc     (new bitmap-dc% [bitmap canvas])])
               (cond [(string=? "" adTxt) (nopublish)])
               (send dc draw-rectangle
                     0   10  ; Top-left at (0, 10), 10 pixels down from top-left
                     350 80) ; width and height 
               (send dc draw-text adTxt
                     10  20)
               (send dc draw-bitmap pic
                     10 100) ; superimpose the bmp
               (publish canvas))))

; let's have some ads as well. 

(implement MakeAd
           (lambda (ip)   ; no publish function, because WhenRequired.
             (let ([txt (ip)])
               (if (string=? txt "") ""
                   (~a "showing Ad for IP " txt)))))

;;TODO match up with diaspec in paper: that is, camera is publishing
;;device, no Button.
