#lang racket/gui


; provide open-firmata, set-pin-mode!, set-arduino-pin!, clear-arduino-pin!

(provide open-asip
         close-asip
         set-pin-mode
         digital-write
         ;         analog-write
         digital-read
         ;         analog-read
         ;         set-autoreport
         set-arduino-pin! ;; shorthand for digital-write, for backward compatibility
         clear-arduino-pin! ;; shorthand for digital-write, for backward compatibility
         set-pin-mode! ;; synonym of set-pin-mode, for backward compatibility
         
         ;; Quickly hacked together for NeoPixels test.
         ;; TODO: we should have a list of services that is built at run-time
         ;; (see Java version)
         ;         set-pixel-color
         ;         set-strip-brightness
         ;         show-strip
         ;; -- End of neopixels --
         
         ;; pin modes
         ;         UNKNOWN_MODE
         INPUT_MODE
         INPUT_PULLUP_MODE
         OUTPUT_MODE
         ;         ANALOG_MODE
         ;         PWM_MODE
         ;         RESERVED_MODE
         ;         OTHER_SERVICE_MODE
         
         ;; Arduino HIGH and LOW (1 and 0)
         HIGH
         LOW
         
         ;; Myrtle-specific function
         ;         w1-stopMotor
         ;         w2-stopMotor
         ;         stopMotors
         ;         setMotor
         ;         setMotors
         ;         readCount
         ;         getCount
         ;         resetCount
         ;         getIR
         ;         leftBump?
         ;         rightBump?
         ;         enableIR
         ;         enableBumpers
         ;         enableCounters
         )
(define OFF #f)
(define ON #t)

(define LOW #f)
(define HIGH #t)

(define INPUT_MODE 0)
(define INPUT_PULLUP_MODE 0)
(define OUTPUT_MODE 0)

; pins 11, 12, 13 =  Red, Amber, Green
; pins 7, 8, 9  =  Red, Amber, Green

; 6, 7 buttons
(define button-1-pin 5)
(define button-2-pin 6)

;(define pins (list 11 10 9))
;(define colours (list "red" "orange" "green"))

(define lights-config (list
                       (list '(11 "red") '(12 "orange") '(13 "green"))
                       (list '(7 "red") '(8 "orange") '(9 "green"))
                       (list '(2 "red") '(3 "orange") '(4 "green"))
                       )
  )

(define get-pins (lambda (light-list) (foldl (lambda (p s) (set-add s (first p))) (set) light-list)))
(define get-all-pins (lambda (lights-conf)
                       (foldl (lambda (ll s) (set-union (get-pins ll) s)) (set) lights-conf)))


; Create UI elements
(define frame (new frame% [label "Traffic Lights"] 
                   [width 300]
                   [height 300]))

(define traffic-light%
  (class canvas% ; The base class is canvas%
    (init-field (col "black"))
    (init-field (pin 10))
    (define/public fill-light (lambda (dc) 
                                ; cond - pin set?
                                (send dc set-brush col (cond [ (hash-ref pin-state pin) 'solid] (else 'transparent)))
                                (send dc draw-ellipse 10 10 50 50)
                                ))
    (super-new)))


(define hpan (new horizontal-panel% [parent frame] [ border 1]))

(define make-light (lambda (p c pan) 
                     (new traffic-light% [parent pan] [col c] [pin p] [ vert-margin 1] [ horiz-margin 1]
                          [min-height 75 ] [ stretchable-height #f ][min-width 75 ] [ stretchable-width #f ]
                          [paint-callback  (lambda (canvas dc) (send canvas fill-light dc) )]
                          ) )) 

(define make-lights (lambda (light-list pan) 
                      (cond ((empty? light-list) null)
                            (else 
                             (make-light (first (first light-list))(second (first light-list)) pan)
                             (make-lights (rest light-list) pan)
                             ) 
                            )
                      )
  
  )

(define make-all-lights (lambda (lights-conf)
                          (cond ((empty? lights-conf) null)
                                (else 
                                 (let ([ vpan (new vertical-panel% [parent hpan] [border 2])])
                                   (make-lights (first lights-conf) vpan)
                                   (make-all-lights (rest lights-conf))
                                   ) 
                                 )
                                )
                          )
  )

(make-all-lights lights-config)

(define refresh-lights (lambda()(for-each  (lambda (traffl) (send traffl refresh-now)) 
                                           (foldl 
                                            (lambda (pan l ) (append (send pan get-children) l)) 
                                            (list)  
                                            (send hpan get-children))
                                           )))

(define button-panel (new horizontal-panel% [parent frame] [ border 1]))

(define makeButton (lambda (lab pin)
                     (new button% [parent button-panel][label lab]
                          [callback (lambda (b e) 
                                      (hash-set! pin-state pin HIGH) ;  set it HIGH
                                      (sleep 0.5)
                                      (hash-set! pin-state pin LOW)  ; set it LOW again
                                      )]
                          ))
  )

(makeButton "Button 1"  button-1-pin)
(makeButton "Button 2"  button-2-pin)

;(new button% [parent button-panel][label "Button 1"]
;     [callback (lambda (b e) 
;                 (hash-set! pin-state button-1-pin HIGH) ;  set it HIGH
;                 (sleep 0.5)
;                 (hash-set! pin-state button-1-pin LOW)  ; set it LOW again
;                )]
;     )
;
;(new button% [parent button-panel][label "Button 2"]
;     [callback (lambda (b e) 
;                 (hash-set! pin-state button-2-pin HIGH) ;  set it HIGH
;                 (sleep 0.5)
;                 (hash-set! pin-state button-2-pin LOW) ; set it LOW again
;                 )]
;     )

; store pin states - inputs and outputs
(define pin-state (make-hash))

; why do this here? why not in set-pin-mode?
(for-each (lambda (e) (hash-set! pin-state e OFF)) (set->list (get-all-pins lights-config)))

; ASIP interface

(define digital-read (lambda (pin) 
                       (hash-ref pin-state pin LOW)))

(define digital-write (lambda (pin value) 
                        (hash-set! pin-state pin value) 
                        (refresh-lights)))

(define set-arduino-pin! (lambda (pin) 
                           (digital-write pin ON)))

(define clear-arduino-pin! (lambda (pin)
                             (digital-write pin OFF)))

(define set-pin-mode (lambda (pin mode) (hash-set! pin-state pin OFF)))
(define set-pin-mode! set-pin-mode)

;(define report-digital-port! (lambda (pin st) null)) 

;(define open-asip (lambda () (thread (send frame show #t))))
(define open-asip (lambda () (send frame show #t)))
(define close-asip (lambda () null)); close the window?

;(define on-button-pressed (lambda (pin fn) (hash-set! button-pins pin fn)))
