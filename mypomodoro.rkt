#lang racket/gui

(require "settings.rkt"
         "nextcloud.rkt"
         "database.rkt"
         "stats.rkt")

(module+ test (require rackunit))

(define (pad n) ( ~r #:min-width 2 n #:pad-string "0"))
(module+ test (check-equal? (pad 2) "02")
         (check-equal? (pad 42) "42"))

(define (seconds->str seconds)
  (let ([secs (remainder seconds 60)]
        [mins (quotient seconds 60)])
    (string-append (pad mins) ":" (pad secs))))
(module+ test (check-equal? (seconds->str 2) "00:02")
         (check-equal? (seconds->str 133) "02:13"))

(define (str-max str max)
  (if (> (string-length str) max)
      (string-append (substring str 0 (- max 3)) "...")
      str))

(define max-choice-len 40)
(define interval (* 60 25))
;(define interval (* 6 1))
(define count interval)
(define running? #f)

; Make a frame by instantiating the frame% class
(define frame (new frame% [label "My Pomodoro"]
                   [stretchable-width #f]))

(define h-panel (new horizontal-panel% [parent frame]))
(define left-panel (new group-box-panel%
                        [label "Task"]
                        [min-width 350]
                        [spacing 2]
                        [border 15]
                        [horiz-margin 15]
                        [vert-margin 15]
                        [alignment '(left top)]
                        [parent h-panel]
                        ))
(define right-panel (new panel% [parent h-panel]
                         [min-width 230]))
(define boards-choice (new choice% [label #f]
                    [choices '("Check Nextcloud Settings")]
                    [min-width 150]
                    [stretchable-width #t]
   	 	    [parent left-panel]
                    [style '(vertical-label)]
                    [callback (lambda (c _)
                                (populate-stacks-choice (send c get-selection)))]))

(define stacks-choice (new choice% [label #f]
                    [choices '()]
                    [min-width 150]
                    [stretchable-width #t]
   	 	    [parent left-panel]
                    [style '(vertical-label)]
                    [callback (lambda (c _)
                                (populate-cards-choice (send c get-selection)))]))

(define cards-choice (new choice% [label #f]
                   [choices '()]
                   [min-width 150]
                   [stretchable-width #t]
                   [style '(vertical-label)]
                   [parent left-panel]))

; Make a static text message in the frame
(define msg (new message% [parent right-panel]
                 [label (seconds->str interval)]
                 [font (make-object font% 40 'default)]))

(define (populate-boards-choice)
  (send boards-choice clear)
  (send stacks-choice clear)
  (send cards-choice clear)  
  (send boards-choice append "Select Board")
  (map
   (lambda (item) (send boards-choice append (str-max (cdr item) max-choice-len)))
   (load-boards-using-prefs)))

(define (populate-stacks-choice boards-selected-index)
  (send stacks-choice clear)
  (send cards-choice clear)    
  (when (> boards-selected-index 0)
    (send stacks-choice append "Select Stack")
    (let ([board-id (car (list-ref (current-boards) (- boards-selected-index 1)))])
      (map
       (lambda (item) (send stacks-choice append (str-max (hash-ref item 'title) max-choice-len) ))
       (load-stacks-using-prefs board-id)))))

(define (populate-cards-choice stacks-selected-index)
  (send cards-choice clear)
  (when (> stacks-selected-index 0)
    (send cards-choice append "Select Card")  
    (let ([stack (list-ref (current-stacks) (- stacks-selected-index 1))])
      (map
       (lambda (item) (send cards-choice append (str-max (hash-ref item 'title) max-choice-len)))
       (hash-ref stack 'cards)))))

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
  (save-pomodoro)
  (play-sound "beep.wav" #t))

(define (save-pomodoro)
  (let ([b (send boards-choice get-selection)]
        [s (send stacks-choice get-selection)]
        [c (send cards-choice get-selection)])
    (when (and (> b 0)
               (> s 0)
               (> c 0))
      (let* ([board (cdr (list-ref (current-boards) (- b 1) ))]
             [stack-info (list-ref (current-stacks) (- s 1))]
             [stack (hash-ref stack-info 'title)]
             [card (hash-ref (list-ref (hash-ref stack-info 'cards) (- c 1))
                             'title)])
        (add-to-logs board stack card)))))

(define timer (new timer% [notify-callback timer-tick]))

(define timer-btn (new button% [parent frame]
                       [label "START"]
                       [callback (lambda (_ __) (if running? (timer-stop) (timer-start)))])) 

(define menu-bar (new menu-bar%
                      (parent frame)))

(define settings-menu (new menu%
                       (label "Settings")
                       (parent menu-bar)))

(define stats-menu (new menu%
                       (label "Stats")
                       (parent menu-bar)))

(define nextcloud-menu (new menu-item%
                          (label "Nextcloud")
                          (parent settings-menu)
                          [callback (lambda (_ __)
                                      (show-settings populate-boards-choice))]))
(define weekly-stats-menu (new menu-item%
                          (label "Weekly Stats")
                          (parent stats-menu)
                          [callback (lambda (_ __)
                                      (plot-weekly-stats))]))

(populate-boards-choice)
(send frame show #t)
