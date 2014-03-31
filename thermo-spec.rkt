#lang s-exp "diaspec.rkt"

; these sources and actions should be defined elsewhere, in a central taxonomy.
(taxonomy "taxo.rkt")

((define-context DesiredTemperature Integer [when-required
                                             get Dial])
 
 (define-context Thermostat Boolean [when-provided Temperature
                                                   get DesiredTemperature
                                                   maybe_publish])
 
 (define-context BoringTest Integer [when-provided Temperature
                                                   get nothing
                                                   always_publish]))

((define-controller FanSpeed [when-provided Thermostat
                                            do Fan]))