#lang s-exp "diaspec.rkt"

; the sources and actions should be defined elsewhere, in a central taxonomy.
(taxonomy "taxo.rkt")

(define-context MakeAd String [when-required get IP])

(define-context ProcessPicture Picture [when-provided Button
                                                      get Camera
                                                      maybe_publish])

(define-context DisplayApp Picture   [when-provided ProcessPicture 
                                                    get MakeAd 
                                                    always_publish])

(define-controller ScreenController [when-provided DisplayApp do Screen])