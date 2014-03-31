;; in this file we will give the implementations of the 
;; taxonomy-defined resources.

(implement Fan
           (lambda (spd)
             (match spd
               [0    (display-line "[Fan] switched off.")]
               [else (display-line "[Fan] speed set to: " spd)])))

(implement Temperature
           (lambda ()
             (display "[TempSens] ambient temp. now? ")
             (let ([val (read)])
               (match (number? val)
                 [#t   val]
                 [else 0  ] ; default value
                 ))))

(implement Dial
           (lambda ()
             ; "the user wants the temperature to be.. 20 deg!"
             20
             ))