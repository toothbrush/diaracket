#lang s-exp diaracket/thermo-spec

;; here we will provide the implementations of the
;; various components.

; import the available resources:
(taxonomy "taxo-impl.rkt")

(implement FanSpeed
           (lambda (warnCtxResult fancontrolspeed)
             (when warnCtxResult 
               (fancontrolspeed 5)
               ;wait
               ;wait
               (fancontrolspeed 1)
               )
             (unless warnCtxResult
               (display-line "[FanSpeed] turning off.")
               (fancontrolspeed 0)
               )
             ))

(implement DesiredTemperature ; when-required ~= always_publish, so final value == return
           (lambda (queryDial)
             (+ (queryDial) 30) ; add 30 just because
             ))

(implement Thermostat ; OTOH, this is best modeled by a continuation
           (lambda (temp getDesiredTemp publish nopublish)
             (display-line "[Thermostat] entering ")
             (let* ([target (getDesiredTemp)] ; only pull data requirement once.
                    [diff   (temp . - . target)]
                    ) 
               (when (temp . > . target) ; temperature too high!
                 (if (diff . > . 20)
                     (publish #t) ; only turn on fan if we're >20 over target
                     (nopublish)))
               (unless (temp . > . target) ; no problem.
                 (display-line "[Thermostat] low temp. => off")
                 (publish #f))
               ; since continuations should act like "return" statements,
               ; control should never get back here. thus, the developer may 
               ; harmlessly do evil shizzle after this point.
               (display-line "[Thermostat] doing evil shizzle!!!"))))

(implement BoringTest
           (lambda (temp publish)
             (publish (+ temp 234))
             ; note that publish does not return control to this function!
             ; if we don't publish, and we use the none/c contract,
             ; we get a run-time error after the context is finished evaluating.
             (display-line "[Test] wiping your hard drive!!!")
             #f))
