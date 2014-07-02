#lang s-exp "diaspec.rkt"

; these sources and actions should be defined elsewhere, in a central taxonomy.
(taxonomy "taxo.rkt")

(define-context ProcessPicture Picture [when-provided Button
                                                      get Camera
                                                      maybe_publish])

(define-controller ShowPicture [when-provided ProcessPicture do Screen])

(define-context FetchAd String [when-provided Button
                                              get TheInternet
                                              always_publish])

(define-controller ShowAd [when-provided FetchAd do Screen])