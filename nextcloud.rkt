#lang racket

(require framework/preferences
         net/base64
         net/url
         json
         "defaults.rkt")

(provide (all-defined-out))

(define current-boards (make-parameter '()))
(define current-stacks (make-parameter '()))

(define (make-auth-header user pass)
  (string-append
       "Authorization: Basic "
       (bytes->string/utf-8
        (base64-encode
         (string->bytes/utf-8
          (string-append user ":" pass))))))

(define (get-deck-remote server user pass api)
  (with-handlers ([exn:fail? (lambda (exn)
                               (displayln (exn-message exn))
                               '())])
    (read-json
     (get-pure-port
      (string->url (string-append server "/index.php/apps/deck/api/v1.0" api)) 
      (list (make-auth-header user pass) "OCS-APIRequest: true")))))

(define (get-boards server user pass)
  (get-deck-remote server user pass "/boards"))

(define (get-stacks server user pass board-id)
  (get-deck-remote server user pass (string-append "/boards/" (number->string board-id) "/stacks")))

(define (load-boards server user pass)
  (current-boards (map (lambda (item)
       (cons (hash-ref item 'id)
             (hash-ref item 'title)))
                       (get-boards server user pass)))
  (current-boards))

(define (load-stacks server user pass board-id)
  (current-stacks (filter
                   (lambda (item) (hash-has-key? item 'cards))
                   (get-stacks server user pass board-id)))
  (current-stacks))

(define (load-boards-using-prefs)
  (load-boards (preferences:get 'server )
               (preferences:get 'user )
               (preferences:get 'pass )))

(define (load-stacks-using-prefs board-id)
    (load-stacks (preferences:get 'server )
                (preferences:get 'user )
                (preferences:get 'pass )
                board-id))
