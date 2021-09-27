#lang racket/gui

(require framework/preferences
         "defaults.rkt")

(provide show-settings
         nc-settings-saved?)

(define (nc-settings-saved?)
  (and
   (not (equal? (hash-ref defaults 'server) (preferences:get 'server)))
   (not (equal? (hash-ref defaults 'user) (preferences:get 'user)))
   (not (equal? (hash-ref defaults 'pass) (preferences:get 'pass)))))

(define (show-settings cb)
  (define settings-frame (new frame% [label "Settings"]
                              [width 400]))
  
  (define nc-panel (new group-box-panel%
                      (parent settings-frame)
                      [alignment '(right top)]
                      [vert-margin 15]
                      [horiz-margin 15]
                      [border 15]
                      [spacing 5]
                      (label "Nextcloud Settings")))
  
  (define server (new text-field%
                    (label "Server")
                    (parent nc-panel)
                    [min-width 150]                                              
                    (init-value (preferences:get 'server))))

  (define user (new text-field%
                      (label "Username")
                      [min-width 150]
                      (parent nc-panel)
                      (init-value (preferences:get 'user))))
  
  (define pass (new text-field%
                      (label "Password")
                      [min-width 150]
                      (style '(single password))
                      (parent nc-panel)
                      (init-value (preferences:get 'pass))))

  (define nc-btn (new button% [parent settings-frame]
                    [label "Save"]
                    [callback (lambda (_ __)
                                (preferences:set 'server (send server get-value))
                                (preferences:set 'user (send user get-value))
                                (preferences:set 'pass (send pass get-value))
                                (send settings-frame show #f)
                                (cb)
                                )]))

  (send settings-frame show #t))
