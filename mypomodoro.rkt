#lang racket/gui

(module+ test (require rackunit))

; Make a frame by instantiating the frame% class
(define frame (new frame% [label "My Pomodoro"]
                   [width 200]))

(define (pad n) ( ~r #:min-width 2 n #:pad-string "0"))
(module+ test
  (check-equal? (pad 2) "02")
  (check-equal? (pad 42) "42"))

(define (seconds->str seconds)
  (let ([secs (remainder seconds 60)]
        [mins (quotient seconds 60)])
    (string-append (pad mins) ":" (pad secs))))
(module+ test
  (check-equal? (seconds->str 2) "00:02")
  (check-equal? (seconds->str 133) "02:13"))

(define interval (* 60 25))
(define count interval)
(define running? #f)

; Make a static text message in the frame
(define msg (new message% [parent frame]
                 [label (seconds->str interval)]
                 [font (make-object font% 40 'default)]))

(define (timer-start)
  (set! running? #t)
  (send timer-btn set-label "STOP")
  (send timer start 1000))

(define (timer-stop)
  (set! running? #f)
  (send timer-btn set-label "START")
  (send timer stop))

(define (timer-tick)
  (set! count (- count 1))
  (send msg set-label (seconds->str count))
  (when (= count 0) (timer-end)))

(define (timer-end)
  (timer-stop)
  (set! count interval)
  (send msg set-label (seconds->str interval))
  (play-sound "ding.mp3" #t))

(define timer (new timer% [notify-callback timer-tick]))

; Make a button in the frame
(define timer-btn (new button% [parent frame]
             [label "START"]
             [callback (lambda (_ __) (if running? (timer-stop) (timer-start)))])) 
 
; Show the frame by calling its show method
(send frame show #t)
