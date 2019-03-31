#lang racket
;(require "AsipMain.rkt" "AsipButtons.rkt")

(require "AsipSimulated.rkt" "AsipButtonsSimulated.rkt")

(define lights (list 11 12 13 7 8 9 2 3 4 )) ; LEDs: (Red, Amber, Green, Red, Amber, Green, Red, Amber, Green)

(define stop-button-pin 6)

(define setup 
  (λ ()
    ;; connect to the Arduino:
    (open-asip)  
    ;; Set pins to OUTPUT_MODE
    (map (λ (x) (set-pin-mode x OUTPUT_MODE)) lights)
    
    ;; Turn the  lights off
    (map (λ (x) (digital-write x LOW)) lights)

    ; set button pin as inputs
    (set-pin-mode! stop-button-pin INPUT_MODE)

    )
  )

(define currentState 1)
;; The button hash, what happens when you click the button on state x (hash "b" y), will change to state y.
(define states (hash 
                    1 (hash "b" 2 "s" 4 )
                    2 (hash "b" 3 "s" 4 )
                    3 (hash "b" 4 "s" 4 )
                    4 (hash "b" 5 "s" 4 )
                    5 (hash "b" 6 "s" 4 )
                    6 (hash "b" 7 "s" 4 )
                    7 (hash "b" 8 "s" 4 )
                    8 (hash "b" 9 "s" 4 )
                    9 (hash "b" 10 "s" 4 )
                    10 (hash "b" 11 "s" 4 )
                    11 (hash "b" 12 "s" 4 )
                    12 (hash "b" 1 "s" 4 )
                    )
  )


(define lightStates (hash 
                     1 (list 1 0 0  1 0 0  1 0 0)  
                     2 (list 1 1 0  1 0 0  1 0 0) 
                     3 (list 0 0 1  1 0 0  1 0 0) 
                     4 (list 0 1 0  1 0 0  1 0 0)
                     5 (list 1 0 0  1 0 0  1 0 0) 
                     6 (list 1 0 0  1 1 0  1 0 0) 
                     7 (list 1 0 0  0 0 1  1 0 0) 
                     8 (list 1 0 0  0 1 0  1 0 0)                     
                     9 (list 1 0 0  1 0 0  1 0 0)
                     10 (list 1 0 0  1 0 0  1 1 0)
                     11 (list 1 0 0  1 0 0  0 0 1)
                     12 (list 1 0 0  1 0 0  0 1 0) 
                              ))

(define getNextState (λ (state event) 
  (hash-ref (hash-ref states state 0) event 0)
  ))
         
(define setLights (λ (lightPins vals) 
                    (cond [(not (empty? lightPins))
                           (cond [(equal? (first vals) 1)
                                  (digital-write (first lightPins) HIGH)
                                  (printf "Setting light ~s to HIGH\n" (first lightPins) )
                                  ]
                                 (else
                                  (digital-write (first lightPins) LOW)
                                  (printf "Setting light ~s to LOW\n" (first lightPins) )
                                  )
                                 )
                           (setLights (rest lightPins) (rest vals))
                           ]
                          )
                    )
  )

(define (state)
  (setLights lights (hash-ref lightStates currentState))
  )

(define nextState (λ (ev)
  (let ([newState (getNextState currentState ev)])
    (cond [ (not (equal? newState 0))
            (set! currentState newState)
           (state)
           (printf "new state ~a\n"  currentState)
           ])
    )
  ))

(on-button-pressed stop-button-pin (λ () (((printf "Button Pressed\n") (test)))))

(define test (λ () (cond
                   ((on-button-pressed stop-button-pin #t) (CheckState))
                   ((#t ((sleep 5)
                   (nextState "b")
                   (test)))))))

(define sequence (λ ()
                   (sleep 5)
                   (nextState "b")
                   (test)
                   ))

(define CheckState (λ () (
                           ( if ( = currentState 1) (RedCross) (print ""))
                           ( if ( = currentState 2) (State2-Red) (print ""))
                           ( if ( = currentState 3) (State3-Red) (print ""))
                           ( if ( = currentState 4) (State4-Red) (print ""))
                          ( if ( = currentState 5) (RedCross) (print "")) ;;All read
                           ( if ( = currentState 6) (State6-Red) (print ""))
                           ( if ( = currentState 7) (State7-Red) (print ""))
                          ( if ( = currentState 8) (State8-Red) (print ""))
                          ( if ( = currentState 9) (RedCross) (print "")) ;;All read
                          ( if ( = currentState 10) (State10-Red) (print ""))
                           ( if ( = currentState 11) (State11-Red) (print ""))
                            ( if ( = currentState 12) (State12-Red) (print "")))))



(define State2-Red (λ () (
                          (sleep 5)
                          (nextState "b")
                          (sleep 5)
                          (nextState "b")
                          (sleep 5)
                          (nextState "b")
                          (sleep 10)
                          (sequence))))

(define State3-Red (λ () (
                          (sleep 5)
                          (nextState "b")
                          (sleep 5)
                          (nextState "b")
                          (sleep 10)
                          (sequence))))

(define State4-Red (λ () (
                          (sleep 5)
                          (nextState "b")
                          (sleep 10)
                          (sequence))))

(define State6-Red (λ () (
                          (sleep 5)
                          (nextState "b")
                          (sleep 5)
                          (nextState "b")
                          (sleep 5)
                          (nextState "b")
                          (sleep 10)
                          (sequence))))
(define State7-Red (λ () (
                          (sleep 5)
                          (nextState "b")
                          (sleep 5)
                          (nextState "b")
                          (sleep 10)
                          (sequence))))

(define State8-Red (λ () (
                          (sleep 5)
                          (nextState "b")
                          (sleep 10)
                          (sequence))))



(define State10-Red (λ () (
                          (sleep 5)
                          (nextState "b")
                          (sleep 5)
                          (nextState "b")
                          (sleep 5)
                          (nextState "b")
                          (sleep 10)
                          (sequence))))

(define State11-Red (λ () (
                          (sleep 5)
                          (nextState "b")
                          (sleep 5)
                          (nextState "b")
                          (sleep 10)
                          (sequence))))

(define State12-Red (λ () (
                          (sleep 5)
                          (nextState "b")
                          (sleep 10)
                          (sequence))))

(define RedCross (λ () (
                          (sleep 10)
                          (sequence)
                          )))



(setup)
(state)
;(sequence)