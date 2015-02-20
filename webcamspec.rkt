#lang s-exp "diaspec.rkt"

; the sources and actions should be defined elsewhere, in a central taxonomy.
(taxonomy "taxo.rkt")

(define-context MakeAd String          [when-required get IP])

(define-context ProcessPicture Picture [when-provided Button
                                                      get Camera
                                                      always_publish])

(define-context ComposeDisplay Picture [when-provided ProcessPicture 
                                                      get MakeAd 
                                                      maybe_publish])

(define-controller Display             [when-provided ComposeDisplay 
                                                      do Screen])