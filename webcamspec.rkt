#lang s-exp "diaspec.rkt"

; the sources and actions should be defined elsewhere, in a central taxonomy.
(taxonomy "taxo.rkt")

(define-context MakeAd
  as String [when required (get IP)])

(define-context ProcessPicture
  as Picture [when provided Button
               (get Camera)
               always-publish])

(define-context ComposeDisplay
  as Picture [when provided ProcessPicture 
               (get MakeAd)
               maybe-publish])

(define-controller Display
  [when provided ComposeDisplay 
    do Screen])