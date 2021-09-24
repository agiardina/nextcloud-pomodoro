
#lang racket/gui

(require net/http-client
         net/base64
         net/url
         json
         )

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


(define (make-auth-header user pass)
  (string-append
       "Authorization: Basic "
       (bytes->string/utf-8
        (base64-encode
         (string->bytes/utf-8
          (string-append user ":" pass)))))) 

(define interval (* 60 25))
(define count interval)
(define running? #f)

; Make a frame by instantiating the frame% class
(define frame (new frame% [label "My Pomodoro"]
                   [width 300]
                   [stretchable-width #f]))

(define h-panel (new horizontal-panel% [parent frame]))
(define left-panel (new group-box-panel% [parent h-panel] [label "Task"]))
(define right-panel (new panel% [parent h-panel]))
(define boards-choice (new choice% [label "Boards"]
                    [choices '()]
                    [min-width 150]
   	 	    [parent left-panel]))
(define stacks-choice (new choice% [label "Stacks"]
                    [choices '()]
                    [min-width 150]
   	 	    [parent left-panel]))
(define cards-choice (new choice% [label "Cards"]
                   [choices '()]
                   [min-width 150]
                   [parent left-panel]))

; Make a static text message in the frame
(define msg (new message% [parent right-panel]
                 [label (seconds->str interval)]
                 [font (make-object font% 40 'default)]))

(define current-boards (make-parameter '()))

(define (get-boards server user pass)
  (get-pure-port
   (string->url (string-append server "/index.php/apps/deck/api/v1.0/boards")) 
   (list (make-auth-header user pass) "OCS-APIRequest: true")))


(define (load-boards server user pass)
  (current-boards (map (lambda (item)
       (cons (hash-ref item 'id)
             (hash-ref item 'title)))
             (read-json (get-boards server user pass)))))

;(map (lambda (item) (send boards-choice append item)) (map cdr (boards)))

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

(define menu-bar (new menu-bar%
                      (parent frame)))

(define file-menu (new menu%
                       (label "&File")
                       (parent menu-bar)))

(define connect-menu (new menu-item%
                          (label "Connect to Nextcloud")
                          (parent file-menu)
                          [callback (lambda (_ __)
                                      (send nc-frame show #t))]))

(define nc-frame (new frame% [label "Nextcloud Settings"]
                      [width 300]))

(define nc-panel (new group-box-panel%
                      (parent nc-frame)
                      [alignment '(right top)]
                      (label "Nextcloud Settings")))

(define server (new text-field%
                    (label "Server")
                    (parent nc-panel)
                    [min-width 150]                                              
                    (init-value "https://cloud.mysite.com")))

(define username (new text-field%
                      (label "Username")
                      [min-width 150]
                      (parent nc-panel)
                      (init-value "")))

(define password (new text-field%
                      (label "Password")
                      [min-width 150]
                      (style '(single password))
                      (parent nc-panel)
                      (init-value "")))

(define nc-btn (new button% [parent nc-frame]
                    [label "Save"]
                    ))

; Show the frame by calling its show method
(send frame show #t)
